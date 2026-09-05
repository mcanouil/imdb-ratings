import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { checkOwner, endSession, getValidToken, SESSION_MAX_MS, sessionFromResponse } from "./auth";

function response(status: number, body: unknown = {}, headers: Record<string, string> = {}): Response {
  return {
    status,
    ok: status >= 200 && status < 300,
    headers: { get: (name: string) => headers[name.toLowerCase()] ?? null },
    json: async () => body,
  } as unknown as Response;
}

/** A minimal in-memory `Storage`, since the `node` test environment has no `localStorage`. */
function createLocalStorage(): Storage {
  const store = new Map<string, string>();
  return {
    getItem: (key: string) => store.get(key) ?? null,
    setItem: (key: string, value: string) => {
      store.set(key, value);
    },
    removeItem: (key: string) => {
      store.delete(key);
    },
    clear: () => {
      store.clear();
    },
    key: (index: number) => Array.from(store.keys())[index] ?? null,
    get length() {
      return store.size;
    },
  } as Storage;
}

beforeEach(() => {
  vi.stubGlobal("fetch", vi.fn());
  vi.stubGlobal("localStorage", createLocalStorage());
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.clearAllMocks();
});

const fetchMock = () => globalThis.fetch as unknown as ReturnType<typeof vi.fn>;

describe("checkOwner", () => {
  it("reports the owner when GitHub returns the owner login", async () => {
    fetchMock().mockResolvedValue(response(200, { login: "mcanouil" }));
    await expect(checkOwner("t")).resolves.toBe("owner");
  });

  it("matches the login case-insensitively", async () => {
    fetchMock().mockResolvedValue(response(200, { login: "McAnouil" }));
    await expect(checkOwner("t")).resolves.toBe("owner");
  });

  it("denies a valid token for another account", async () => {
    fetchMock().mockResolvedValue(response(200, { login: "someone-else" }));
    await expect(checkOwner("t")).resolves.toBe("denied");
  });

  it("reports a rejected token as expired, not denied", async () => {
    fetchMock().mockResolvedValue(response(401));
    await expect(checkOwner("t")).resolves.toBe("expired");
  });

  it("denies a forbidden token that is not rate limited", async () => {
    fetchMock().mockResolvedValue(response(403, {}, { "x-ratelimit-remaining": "42" }));
    await expect(checkOwner("t")).resolves.toBe("denied");
  });

  it("treats a rate-limited response as unreachable, not as a denial", async () => {
    fetchMock().mockResolvedValue(response(403, {}, { "x-ratelimit-remaining": "0" }));
    await expect(checkOwner("t")).resolves.toBe("unreachable");
  });

  it("treats a server error as unreachable", async () => {
    fetchMock().mockResolvedValue(response(500));
    await expect(checkOwner("t")).resolves.toBe("unreachable");
  });

  it("treats a network failure as unreachable", async () => {
    fetchMock().mockRejectedValue(new TypeError("Failed to fetch"));
    await expect(checkOwner("t")).resolves.toBe("unreachable");
  });

  it("treats an unreadable body as unreachable", async () => {
    fetchMock().mockResolvedValue({
      status: 200,
      ok: true,
      headers: { get: () => null },
      json: async () => {
        throw new Error("bad json");
      },
    } as unknown as Response);
    await expect(checkOwner("t")).resolves.toBe("unreachable");
  });
});

const SESSION_KEY = "gh_session";
const DAY_MS = 24 * 3_600_000;

describe("sessionFromResponse", () => {
  it("caps a non-expiring token at seven days", () => {
    const session = sessionFromResponse({ access_token: "t" }, 1_000);
    expect(SESSION_MAX_MS).toBe(7 * DAY_MS);
    expect(session.expires_at).toBe(1_000 + 7 * DAY_MS);
  });

  it("keeps a shorter GitHub expiry when one is supplied", () => {
    const session = sessionFromResponse({ access_token: "t", expires_in: 3_600 }, 1_000);
    expect(session.expires_at).toBe(1_000 + 3_600_000);
  });

  it("treats a zero expiry as immediate, not as non-expiring", () => {
    const session = sessionFromResponse({ access_token: "t", expires_in: 0 }, 1_000);
    expect(session.expires_at).toBe(1_000);
  });

  it("never extends a session beyond the cap", () => {
    const session = sessionFromResponse({ access_token: "t", expires_in: 60 * 24 * 3600 }, 1_000);
    expect(session.expires_at).toBe(1_000 + 7 * DAY_MS);
  });
});

describe("getValidToken", () => {
  it("returns the token for a session inside its window", async () => {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ access_token: "t", expires_at: Date.now() + DAY_MS }));
    await expect(getValidToken()).resolves.toBe("t");
  });

  it("returns null and clears a session past its expiry", async () => {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ access_token: "t", expires_at: Date.now() - 1 }));
    fetchMock().mockResolvedValue({ ok: true, status: 200 } as Response);
    await expect(getValidToken()).resolves.toBeNull();
    expect(localStorage.getItem(SESSION_KEY)).toBeNull();
  });

  it("backfills a stored session that predates the cap", async () => {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ access_token: "old" }));
    await expect(getValidToken()).resolves.toBe("old");
    const stored = JSON.parse(localStorage.getItem(SESSION_KEY)!) as { expires_at?: number };
    expect(stored.expires_at).toBeGreaterThan(Date.now());
  });

  it("backfills a stored session with a non-finite expiry", async () => {
    localStorage.setItem(SESSION_KEY, '{"access_token":"old","expires_at":1e999}');
    await expect(getValidToken()).resolves.toBe("old");
    const stored = JSON.parse(localStorage.getItem(SESSION_KEY)!) as { expires_at?: number };
    expect(Number.isFinite(stored.expires_at)).toBe(true);
    expect(stored.expires_at).toBeGreaterThan(Date.now());
  });
});

describe("endSession", () => {
  it("drops the local session even when the revocation call fails", async () => {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ access_token: "t", expires_at: Date.now() + DAY_MS }));
    fetchMock().mockRejectedValue(new TypeError("Failed to fetch"));
    await expect(endSession()).resolves.toBe(false);
    expect(localStorage.getItem(SESSION_KEY)).toBeNull();
  });

  it("reports success when the gateway revokes the token", async () => {
    localStorage.setItem(SESSION_KEY, JSON.stringify({ access_token: "t", expires_at: Date.now() + DAY_MS }));
    fetchMock().mockResolvedValue({ ok: true, status: 200 } as Response);
    await expect(endSession()).resolves.toBe(true);
    expect(localStorage.getItem(SESSION_KEY)).toBeNull();
  });

  it("does not call the gateway when there is no session to end", async () => {
    await expect(endSession()).resolves.toBe(true);
    expect(fetchMock()).not.toHaveBeenCalled();
  });
});
