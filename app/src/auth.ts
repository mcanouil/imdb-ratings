import { GATEWAY_BASE, OWNER_LOGIN } from "./config";
import { fetchOrThrow } from "./util";

// Persisted across app close/open so the session survives reopening.
// The GitHub App issues tokens that do not expire.
// So this file sets the session lifetime instead of relying on GitHub.
// A session lasts seven days.
// After seven days, `endSession` revokes the session at GitHub.
// The user then signs in again.
// A shorter GitHub expiry, if one is ever enabled, wins over the cap.
const SESSION_KEY = "gh_session";
const EXPIRY_SKEW_MS = 60_000;

/** Longest a session may live on this device before a fresh sign-in is required. */
export const SESSION_MAX_MS = 7 * 24 * 3_600_000;

interface Session {
  access_token: string;
  expires_at: number; // epoch ms; always set, at the earlier of GitHub's expiry and the cap
}

// A session as read from storage. `expires_at` is missing on sessions written
// before the seven-day cap existed, so it is optional here but required on the
// `Session` that is written back.
type StoredSession = { access_token: string; expires_at?: number };

function readSession(): StoredSession | null {
  const raw = localStorage.getItem(SESSION_KEY);
  if (!raw) return null;
  try {
    return JSON.parse(raw) as StoredSession;
  } catch {
    return null;
  }
}

interface TokenResponse {
  access_token?: string;
  expires_in?: number;
  error?: string;
}

// The fields needed to build a session.
// The access token is guaranteed present here.
// `writeSession` narrows a `TokenResponse` down to this type.
// It does this before it calls `sessionFromResponse`.
// So the builder never has to assert the token.
export interface IssuedToken {
  access_token: string;
  expires_in?: number;
}

/** Build the session to store, capping its life at `SESSION_MAX_MS`. */
export function sessionFromResponse(data: IssuedToken, now: number): Session {
  const githubExpiry = typeof data.expires_in === "number" ? now + data.expires_in * 1000 : Number.POSITIVE_INFINITY;
  return {
    access_token: data.access_token,
    expires_at: Math.min(githubExpiry, now + SESSION_MAX_MS),
  };
}

function writeSession(data: TokenResponse): string {
  const token = data.access_token;
  if (!token) throw new Error("Cannot store a session without an access token.");
  const issued: IssuedToken =
    typeof data.expires_in === "number"
      ? { access_token: token, expires_in: data.expires_in }
      : { access_token: token };
  const session = sessionFromResponse(issued, Date.now());
  localStorage.setItem(SESSION_KEY, JSON.stringify(session));
  return session.access_token;
}

function clearToken(): void {
  localStorage.removeItem(SESSION_KEY);
}

/** Return the stored access token if still valid; end the session once it has expired. */
export async function getValidToken(): Promise<string | null> {
  const session = readSession();
  if (!session) return null;

  // Sessions written before the seven-day cap have no expiry.
  // This code gives them one now, so the upgrade does not abruptly end an
  // existing sign-in.
  if (typeof session.expires_at !== "number" || !Number.isFinite(session.expires_at)) {
    const backfilled: Session = { access_token: session.access_token, expires_at: Date.now() + SESSION_MAX_MS };
    localStorage.setItem(SESSION_KEY, JSON.stringify(backfilled));
    return backfilled.access_token;
  }

  if (Date.now() > session.expires_at - EXPIRY_SKEW_MS) {
    void endSession();
    return null;
  }
  return session.access_token;
}

export type OwnerCheck = "owner" | "denied" | "expired" | "unreachable";

/**
 * Resolve whether a token belongs to the owner. Three failure cases stay
 * distinct: a token GitHub rejects outright ("expired"), a valid token
 * belonging to a different login ("denied"), and an unreachable GitHub
 * ("unreachable") so that a network failure never destroys a valid session.
 * api.github.com allows CORS (`*`).
 */
export async function checkOwner(token: string): Promise<OwnerCheck> {
  let res: Response;
  try {
    res = await fetch("https://api.github.com/user", {
      headers: { Authorization: `Bearer ${token}`, Accept: "application/vnd.github+json" },
    });
  } catch {
    return "unreachable";
  }
  if (res.status === 401) return "expired";
  // A 403 is either a real rejection or a rate limit. Only the former is a denial.
  if (res.status === 403) {
    return res.headers.get("X-RateLimit-Remaining") === "0" ? "unreachable" : "denied";
  }
  if (!res.ok) return "unreachable";
  const user = (await res.json().catch(() => null)) as { login?: string } | null;
  if (!user?.login) return "unreachable";
  return user.login.toLowerCase() === OWNER_LOGIN ? "owner" : "denied";
}

export interface DeviceCode {
  device_code: string;
  user_code: string;
  verification_uri: string;
  expires_in: number;
  interval: number;
}

export async function startDeviceFlow(): Promise<DeviceCode> {
  const res = await fetchOrThrow(`${GATEWAY_BASE}/github/device/code`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
  });
  if (!res.ok) throw new Error(`Device-code request failed (HTTP ${res.status}).`);
  return (await res.json()) as DeviceCode;
}

const sleep = (ms: number) => new Promise<void>((r) => setTimeout(r, ms));

/** Poll the gateway until the user authorises the device, then store the session. */
export async function pollForToken(device: DeviceCode): Promise<string> {
  let intervalMs = (device.interval || 5) * 1000;
  const deadline = Date.now() + device.expires_in * 1000;

  while (Date.now() < deadline) {
    await sleep(intervalMs);
    const res = await fetchOrThrow(`${GATEWAY_BASE}/github/token`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ device_code: device.device_code }),
    });
    const data = (await res.json()) as TokenResponse;

    if (data.access_token) return writeSession(data);
    switch (data.error) {
      case "authorization_pending":
        break;
      case "slow_down":
        intervalMs += 5000;
        break;
      case "expired_token":
      case "access_denied":
      case "incorrect_client_credentials":
        throw new Error(`Authorisation failed: ${data.error}.`);
      default:
        if (data.error) throw new Error(`Authorisation failed: ${data.error}.`);
    }
  }
  throw new Error("Device code expired. Restart the login.");
}

/**
 * Ask the gateway to revoke a token at GitHub.
 * This is best effort.
 * It resolves false when the gateway or the network is unavailable.
 * A caller can then report the failure without blocking on it.
 */
export async function revokeToken(token: string): Promise<boolean> {
  try {
    const res = await fetch(`${GATEWAY_BASE}/github/revoke`, {
      method: "POST",
      headers: { Authorization: `Bearer ${token}` },
    });
    return res.ok;
  } catch {
    return false;
  }
}

/**
 * End the session on this device and at GitHub.
 * This function drops the local session first and always.
 * A failed revocation then never leaves a credential behind.
 * The result says whether GitHub also dropped the session.
 */
export async function endSession(): Promise<boolean> {
  const token = readSession()?.access_token ?? null;
  clearToken();
  return token === null ? true : await revokeToken(token);
}
