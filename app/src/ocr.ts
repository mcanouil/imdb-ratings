import { preprocessTicket } from "./preprocess";

export interface OcrResult {
  text: string;
  /** The binarised image actually fed to OCR, for preview. */
  prepared: Blob;
}

/** OCR a captured ticket image with the French model. Tesseract is loaded lazily (multi-MB wasm + traineddata). */
export async function ocrTicket(image: File | Blob, onProgress?: (fraction: number) => void): Promise<OcrResult> {
  const prepared = await preprocessTicket(image).catch(() => image);
  const { PSM, createWorker } = await import("tesseract.js");
  const worker = await createWorker("fra", undefined, {
    logger: (m: { status: string; progress: number }) => {
      if (onProgress && m.status === "recognizing text") onProgress(m.progress);
    },
  });
  try {
    // Layout analysis after binarisation keeps stacked blocks (theatre / date / title) together.
    await worker.setParameters({ tessedit_pageseg_mode: PSM.AUTO });
    const { data } = await worker.recognize(prepared);
    return { text: data.text, prepared };
  } finally {
    await worker.terminate();
  }
}
