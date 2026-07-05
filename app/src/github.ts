import { GATEWAY_BASE } from "./config";
import type { TicketFields } from "./parseTicket";
import { fetchOrThrow } from "./util";

export interface CommitResult {
  html_url: string | null;
  verified: boolean;
}

/** Keep a value on one line and comma-free so it cannot break out of its CSV field. */
function csvField(value: string): string {
  return value.replace(/[\r\n,]+/g, " ").trim();
}

/** Build a CSV row "YYYY-MM-DD HH:MM,room,THEATRE,ttID" with single-line, comma-free fields. */
export function buildRow(fields: TicketFields, imdbId: string): string {
  const date = csvField(fields.date);
  const time = csvField(fields.time);
  const room = fields.room.replace(/\D/g, "");
  const theatre = csvField(fields.theatre).toUpperCase();
  // IMDb ids are "tt" followed by digits; keep only that leading shape so a crafted
  // suggestion payload cannot smuggle a comma or newline (extra columns / rows).
  const id = imdbId.match(/^tt\d+/)?.[0] ?? "";
  return `${date} ${time},${room},${theatre},${id}`;
}

/**
 * Commit the row via the gateway, which mints a repo-scoped GitHub App
 * installation token so the commit is GitHub-signed (Verified). The user's
 * device-flow token is sent only as an identity gate.
 */
export async function commit(userToken: string, row: string): Promise<CommitResult> {
  const res = await fetchOrThrow(`${GATEWAY_BASE}/github/commit`, {
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
