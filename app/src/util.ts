/** Remove diacritics so OCR'd "février"/"fevrier" and "Amélie"/"Amelie" compare equal. */
export function stripDiacritics(input: string): string {
  return input.normalize("NFD").replace(/\p{Diacritic}/gu, "");
}
