import { GATEWAY_BASE } from "./config";
import type { TicketFields } from "./parseTicket";

export interface CommitResult {
  html_url: string | null;
  verified: boolean;
}

/** Build a CSV row "YYYY-MM-DD HH:MM,room,THEATRE,ttID". */
export function buildRow(fields: TicketFields, imdbId: string): string {
  return `${fields.date} ${fields.time},${fields.room},${fields.theatre},${imdbId}`;
}

/**
 * Commit the row via the gateway, which mints a repo-scoped GitHub App
 * installation token so the commit is GitHub-signed (Verified). The user's
 * device-flow token is sent only as an identity gate.
 */
export async function commit(userToken: string, row: string): Promise<CommitResult> {
  const res = await fetch(`${GATEWAY_BASE}/github/commit`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: `Bearer ${userToken}` },
    body: JSON.stringify({ row }),
  });
  if (!res.ok) {
    let detail = `HTTP ${res.status}`;
    try {
      const body = (await res.json()) as { error?: string };
      if (body.error) detail = body.error;
    } catch {
      // non-JSON error body; keep the status code
    }
    throw new Error(`Commit failed: ${detail}`);
  }
  return (await res.json()) as CommitResult;
}
