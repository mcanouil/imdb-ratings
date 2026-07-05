// Vendor the Tesseract OCR runtime (worker + SIMD-LSTM core wasm + French
// traineddata) into app/public/vendor so the browser loads them same-origin
// instead of from third-party CDNs. Keeps the app off cdn.jsdelivr.net /
// unpkg.com / tessdata.projectnaptha.com, so a compromised CDN cannot inject
// code into the owner's authenticated session.
//
// Re-run after upgrading tesseract.js: `npm run vendor:ocr`.
// The core JS/wasm are copied from the installed tesseract.js-core so they always
// match the resolved version. The traineddata is pinned to the data version that
// tesseract.js 7 requests for LSTM_ONLY mode (4.0.0_best_int).

import { createRequire } from "node:module";
import { mkdir, copyFile, writeFile, access } from "node:fs/promises";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const here = dirname(fileURLToPath(import.meta.url));
const publicDir = join(here, "..", "public", "vendor");
const tesseractDir = join(publicDir, "tesseract");
const tessdataDir = join(publicDir, "tessdata");

// The app runs createWorker("fra") with the default LSTM_ONLY engine and forces
// the SIMD-LSTM core (see ocr.ts), so only these core files are needed.
const CORE_FILES = ["tesseract-core-simd-lstm.wasm.js", "tesseract-core-simd-lstm.wasm"];
const CORE_ROOT = dirname(require.resolve("tesseract.js-core/package.json"));
const WORKER_SRC = require.resolve("tesseract.js/dist/worker.min.js");

// LSTM_ONLY mode fetches "<lang>.traineddata.gz" from the 4.0.0_best_int dataset.
const TRAINEDDATA_URL = "https://cdn.jsdelivr.net/npm/@tesseract.js-data/fra/4.0.0_best_int/fra.traineddata.gz";
const TRAINEDDATA_DEST = join(tessdataDir, "fra.traineddata.gz");

const exists = async (path) => access(path).then(() => true, () => false);

await mkdir(tesseractDir, { recursive: true });
await mkdir(tessdataDir, { recursive: true });

await copyFile(WORKER_SRC, join(tesseractDir, "worker.min.js"));
for (const file of CORE_FILES) {
  await copyFile(join(CORE_ROOT, file), join(tesseractDir, file));
}
console.log(`Copied worker + core into ${tesseractDir}`);

if (await exists(TRAINEDDATA_DEST)) {
  console.log("fra.traineddata.gz already present; skipping download.");
} else {
  const res = await fetch(TRAINEDDATA_URL);
  if (!res.ok) throw new Error(`Failed to download traineddata (HTTP ${res.status}) from ${TRAINEDDATA_URL}`);
  await writeFile(TRAINEDDATA_DEST, Buffer.from(await res.arrayBuffer()));
  console.log(`Downloaded fra.traineddata.gz into ${tessdataDir}`);
}
