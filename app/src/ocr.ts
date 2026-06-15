/** OCR a captured ticket image with the French model. Tesseract is loaded lazily (multi-MB wasm + traineddata). */
export async function ocrTicket(image: File | Blob, onProgress?: (fraction: number) => void): Promise<string> {
  const { recognize } = await import("tesseract.js");
  const { data } = await recognize(image, "fra", {
    logger: (m: { status: string; progress: number }) => {
      if (onProgress && m.status === "recognizing text") onProgress(m.progress);
    },
  });
  return data.text;
}
