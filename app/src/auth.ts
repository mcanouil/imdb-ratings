import { GATEWAY_BASE, OWNER_LOGIN } from "./config";

// Persisted across app close/open so the session survives reopening, but only the short-lived
// access token is stored (no long-lived refresh token): reopening within the token's lifetime
// skips login; once it expires (~8h with expiring user tokens) the user signs in again.
const SESSION_KEY = "gh_session";
const EXPIRY_SKEW_MS = 60_000;

interface Session {
  access_token: string;
  expires_at?: number; // epoch ms; absent when tokens do not expire
}

interface TokenResponse {
  access_token?: string;
  expires_in?: number;
  error?: string;
}

function readSession(): Session | null {
  const raw = localStorage.getItem(SESSION_KEY);
  if (!raw) return null;
  try {
    return JSON.parse(raw) as Session;
  } catch {
    return null;
  }
}

function writeSession(data: TokenResponse): string {
  const session: Session = {
    access_token: data.access_token!,
    expires_at: data.expires_in ? Date.now() + data.expires_in * 1000 : undefined,
  };
  localStorage.setItem(SESSION_KEY, JSON.stringify(session));
  return session.access_token;
}

export function clearToken(): void {
  localStorage.removeItem(SESSION_KEY);
}

/** Return the stored access token if still valid; clear and return null once it has expired. */
export async function getValidToken(): Promise<string | null> {
  const session = readSession();
  if (!session) return null;
  const expired = session.expires_at !== undefined && Date.now() > session.expires_at - EXPIRY_SKEW_MS;
  if (expired) {
    clearToken();
    return null;
  }
  return session.access_token;
}

/** Resolve the login behind a token, or null if invalid. api.github.com allows CORS (`*`). */
export async function fetchLogin(token: string): Promise<string | null> {
  const res = await fetch("https://api.github.com/user", {
    headers: { Authorization: `Bearer ${token}`, Accept: "application/vnd.github+json" },
  });
  if (!res.ok) return null;
  const user = (await res.json()) as { login?: string };
  return user.login ?? null;
}

/** True only when the token belongs to the repo owner. */
export async function isOwner(token: string): Promise<boolean> {
  const login = await fetchLogin(token);
  return login?.toLowerCase() === OWNER_LOGIN;
}

export interface DeviceCode {
  device_code: string;
  user_code: string;
  verification_uri: string;
  expires_in: number;
  interval: number;
}

export async function startDeviceFlow(): Promise<DeviceCode> {
  const res = await fetch(`${GATEWAY_BASE}/github/device/code`, {
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
    const res = await fetch(`${GATEWAY_BASE}/github/token`, {
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
