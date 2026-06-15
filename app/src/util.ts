/** Remove diacritics so OCR'd "février"/"fevrier" and "Amélie"/"Amelie" compare equal. */
export function stripDiacritics(input: string): string {
  return input.normalize("NFD").replace(/\p{Diacritic}/gu, "");
}

/** Best-effort human message from an unknown thrown value. */
export function errorMessage(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}
