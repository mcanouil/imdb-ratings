import { GATEWAY_BASE } from "./config";

const TOKEN_KEY = "gh_token";

export interface DeviceCode {
  device_code: string;
  user_code: string;
  verification_uri: string;
  expires_in: number;
  interval: number;
}

export function getStoredToken(): string | null {
  return sessionStorage.getItem(TOKEN_KEY);
}

export function storeToken(token: string): void {
  sessionStorage.setItem(TOKEN_KEY, token);
}

export function clearToken(): void {
  sessionStorage.removeItem(TOKEN_KEY);
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

/** Poll the gateway until the user authorises the device, then store the token. */
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
    const data = (await res.json()) as { access_token?: string; error?: string };

    if (data.access_token) {
      storeToken(data.access_token);
      return data.access_token;
    }
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
