import { useEffect, useRef, useState } from "react";
import { ocrTicket } from "./ocr";
import { errorMessage } from "./util";

interface Props {
  onResult: (text: string, previewUrl: string) => void;
  onError: (message: string) => void;
}

/** OCR a captured image and turn the binarised result into a preview object URL. */
async function ocrToResult(source: Blob, onProgress: (n: number) => void): Promise<{ text: string; url: string }> {
  const { text, prepared } = await ocrTicket(source, onProgress);
  return { text, url: URL.createObjectURL(prepared) };
}

/** In-app camera (getUserMedia) with a single-shot shutter; falls back to a photo picker. */
export function CameraCapture({ onResult, onError }: Props) {
  const videoRef = useRef<HTMLVideoElement>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const [mode, setMode] = useState<"starting" | "live" | "fallback">("starting");
  const [busy, setBusy] = useState(false);
  const [progress, setProgress] = useState(0);

  const stop = () => {
    streamRef.current?.getTracks().forEach((track) => track.stop());
    streamRef.current = null;
  };

  const start = async () => {
    onError("");
    setMode("starting");
    try {
      streamRef.current = await navigator.mediaDevices.getUserMedia({
        video: { facingMode: { ideal: "environment" } },
        audio: false,
      });
      setMode("live");
    } catch {
      setMode("fallback");
    }
  };

  // Open the camera as soon as the step appears; release it on unmount.
  useEffect(() => {
    void start();
    return stop;
  }, []);

  useEffect(() => {
    if (mode === "live" && videoRef.current && streamRef.current) {
      videoRef.current.srcObject = streamRef.current;
      void videoRef.current.play().catch(() => undefined);
    }
  }, [mode]);

  const shutter = async () => {
    const video = videoRef.current;
    if (!video || busy) return;
    const w = video.videoWidth;
    const h = video.videoHeight;
    if (!w || !h) return;

    const canvas = document.createElement("canvas");
    canvas.width = w;
    canvas.height = h;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;
    ctx.drawImage(video, 0, 0, w, h);

    setBusy(true);
    setProgress(0);
    onError("");
    const blob = await new Promise<Blob | null>((resolve) => canvas.toBlob(resolve, "image/jpeg", 0.95));
    if (!blob) {
      onError("Could not capture the frame.");
      setBusy(false);
      return;
    }
    try {
      const { text, url } = await ocrToResult(blob, setProgress);
      stop();
      onResult(text, url);
    } catch (e) {
      onError(errorMessage(e));
      setBusy(false);
    }
  };

  if (mode === "fallback") return <PhotoCapture onResult={onResult} onError={onError} />;

  if (mode === "starting") {
    return (
      <section className="step">
        <p>Starting camera…</p>
      </section>
    );
  }

  return (
    <div className="camera">
      <video ref={videoRef} className="camera-video" playsInline muted autoPlay />
      <div className="camera-guide" aria-hidden="true" />
      <p className="camera-hint">Frame the ticket inside the guide, then capture.</p>
      <div className="camera-bar">
        {busy ? (
          <span className="camera-status">Reading… {Math.round(progress * 100)}%</span>
        ) : (
          <>
            <button type="button" className="shutter" onClick={shutter} aria-label="Capture ticket" />
            <button
              type="button"
              className="link"
              onClick={() => {
                stop();
                setMode("fallback");
              }}
            >
              Photo
            </button>
          </>
        )}
      </div>
    </div>
  );
}

/** Fallback when getUserMedia is unavailable/denied: the OS photo picker. */
function PhotoCapture({ onResult, onError }: Props) {
  const [busy, setBusy] = useState(false);
  const [progress, setProgress] = useState(0);

  const handleFile = async (file: File) => {
    setBusy(true);
    setProgress(0);
    onError("");
    try {
      const { text, url } = await ocrToResult(file, setProgress);
      onResult(text, url);
    } catch (e) {
      onError(errorMessage(e));
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <p>Pick a photo of the ticket. Fields are extracted then you confirm.</p>
      <label className="capture-button">
        <span className="capture-icon" aria-hidden="true">
          🖼️
        </span>
        <span>{busy ? `Reading… ${Math.round(progress * 100)}%` : "Choose a photo"}</span>
        <input
          type="file"
          accept="image/*"
          capture="environment"
          disabled={busy}
          onChange={(e) => {
            const file = e.target.files?.[0];
            if (file) void handleFile(file);
          }}
        />
      </label>
    </section>
  );
}
