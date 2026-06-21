/** Remove diacritics so OCR'd "février"/"fevrier" and "Amélie"/"Amelie" compare equal. */
export function stripDiacritics(input: string): string {
  return input.normalize("NFD").replace(/\p{Diacritic}/gu, "");
}

/** Best-effort human message from an unknown thrown value. */
export function errorMessage(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

/**
 * `fetch` that turns a network/CORS-level failure into a clear, actionable error.
 * A blocked or unreachable request throws `TypeError` ("Load failed" in WebKit)
 * which never reaches an `!res.ok` check, so translate it before it surfaces raw.
 */
export async function fetchOrThrow(input: RequestInfo | URL, init?: RequestInit): Promise<Response> {
  try {
    return await fetch(input, init);
  } catch (e) {
    if (e instanceof TypeError) {
      throw new Error("Could not reach the server (network or CORS). Check your connection and try again.");
    }
    throw e;
  }
}
