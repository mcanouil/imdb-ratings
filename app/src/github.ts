import { REPO_OWNER, REPO_NAME, REPO_BRANCH, CSV_PATH } from "./config";
import type { TicketFields } from "./parseTicket";

const API = `https://api.github.com/repos/${REPO_OWNER}/${REPO_NAME}/contents/${CSV_PATH}`;

export interface CsvFile {
  content: string;
  sha: string;
}

function encodeBase64(text: string): string {
  const bytes = new TextEncoder().encode(text);
  let binary = "";
  for (const b of bytes) binary += String.fromCharCode(b);
  return btoa(binary);
}

function decodeBase64(b64: string): string {
  const binary = atob(b64.replace(/\s/g, ""));
  const bytes = Uint8Array.from(binary, (c) => c.charCodeAt(0));
  return new TextDecoder().decode(bytes);
}

function headers(token: string): HeadersInit {
  return {
    Authorization: `Bearer ${token}`,
    Accept: "application/vnd.github+json",
    "X-GitHub-Api-Version": "2022-11-28",
  };
}

export async function getCsv(token: string): Promise<CsvFile> {
  const res = await fetch(`${API}?ref=${REPO_BRANCH}`, { headers: headers(token) });
  if (!res.ok) {
    throw new Error(`Could not read ${CSV_PATH} (HTTP ${res.status}). Check the token and that the GitHub App is installed on the repo.`);
  }
  const json = (await res.json()) as { content: string; sha: string };
  return { content: decodeBase64(json.content), sha: json.sha };
}

/** Build a CSV row "YYYY-MM-DD HH:MM,room,THEATRE,ttID". */
export function buildRow(fields: TicketFields, imdbId: string): string {
  return `${fields.date} ${fields.time},${fields.room},${fields.theatre},${imdbId}`;
}

/** Insert a row keeping the file's descending-by-date_time order (newest first). */
export function insertRow(csv: string, row: string): string {
  const newKey = row.split(",")[0];
  const hadTrailingNewline = csv.endsWith("\n");
  const lines = csv.replace(/\n$/, "").split("\n");
  const header = lines[0];
  const body = lines.slice(1);

  let idx = body.findIndex((line) => line.split(",")[0] < newKey);
  if (idx === -1) idx = body.length;
  body.splice(idx, 0, row);

  const out = [header, ...body].join("\n");
  return hadTrailingNewline ? `${out}\n` : out;
}

export async function commit(token: string, content: string, sha: string): Promise<void> {
  const res = await fetch(API, {
    method: "PUT",
    headers: { ...headers(token), "Content-Type": "application/json" },
    body: JSON.stringify({
      message: "chore: update theatres.csv",
      content: encodeBase64(content),
      sha,
      branch: REPO_BRANCH,
    }),
  });
  if (!res.ok) {
    const detail = await res.text();
    throw new Error(`Commit failed (HTTP ${res.status}): ${detail}`);
  }
}
