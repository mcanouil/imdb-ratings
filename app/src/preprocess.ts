// Thermal tickets print faint text over a tiled watermark. Binarising with an
// Otsu threshold removes the watermark and boosts contrast, which markedly
// improves Tesseract accuracy versus feeding the raw photo.

// Downscale large photos less aggressively (preserves small glyphs like
// "Salle 06") and upscale tiny captures toward Tesseract's preferred cap-height.
const TARGET_MAX_DIM = 2400;
const TARGET_MIN_DIM = 1600;

function otsuThreshold(histogram: number[], total: number): number {
  let sum = 0;
  for (let i = 0; i < 256; i += 1) sum += i * histogram[i];
  let sumB = 0;
  let wB = 0;
  let maxVariance = 0;
  let threshold = 127;
  for (let t = 0; t < 256; t += 1) {
    wB += histogram[t];
    if (wB === 0) continue;
    const wF = total - wB;
    if (wF === 0) break;
    sumB += t * histogram[t];
    const mB = sumB / wB;
    const mF = (sum - sumB) / wF;
    const variance = wB * wF * (mB - mF) * (mB - mF);
    if (variance > maxVariance) {
      maxVariance = variance;
      threshold = t;
    }
  }
  return threshold;
}

/** Grayscale + Otsu-threshold the captured ticket; returns a PNG blob for OCR. */
export async function preprocessTicket(image: Blob): Promise<Blob> {
  const bitmap = await createImageBitmap(image);
  const maxDim = Math.max(bitmap.width, bitmap.height);
  let scale = 1;
  if (maxDim > TARGET_MAX_DIM) scale = TARGET_MAX_DIM / maxDim;
  else if (maxDim > 0 && maxDim < TARGET_MIN_DIM) scale = TARGET_MIN_DIM / maxDim;
  const w = Math.round(bitmap.width * scale);
  const h = Math.round(bitmap.height * scale);

  const canvas = document.createElement("canvas");
  canvas.width = w;
  canvas.height = h;
  const ctx = canvas.getContext("2d");
  if (!ctx) return image;
  ctx.imageSmoothingQuality = "high";
  ctx.drawImage(bitmap, 0, 0, w, h);
  bitmap.close();

  const data = ctx.getImageData(0, 0, w, h);
  const pixels = data.data;
  const histogram = new Array<number>(256).fill(0);

  // Pass 1: grayscale (luminance) in place, build histogram.
  for (let i = 0; i < pixels.length; i += 4) {
    const gray = Math.round(0.299 * pixels[i] + 0.587 * pixels[i + 1] + 0.114 * pixels[i + 2]);
    pixels[i] = pixels[i + 1] = pixels[i + 2] = gray;
    histogram[gray] += 1;
  }

  // Pass 2: binarise around the Otsu threshold.
  const threshold = otsuThreshold(histogram, w * h);
  for (let i = 0; i < pixels.length; i += 4) {
    const value = pixels[i] > threshold ? 255 : 0;
    pixels[i] = pixels[i + 1] = pixels[i + 2] = value;
  }
  ctx.putImageData(data, 0, 0);

  return new Promise<Blob>((resolve) => canvas.toBlob((blob) => resolve(blob ?? image), "image/png"));
}
