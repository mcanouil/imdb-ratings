import { useEffect, useRef, useState } from "react";
import { ocrTicket } from "./ocr";

interface Props {
  onResult: (text: string, previewUrl: string) => void;
  onError: (message: string) => void;
}

function message(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

/** In-app camera (getUserMedia) with a single-shot shutter; falls back to a photo picker. */
export function CameraCapture({ onResult, onError }: Props) {
  const videoRef = useRef<HTMLVideoElement>(null);
  const streamRef = useRef<MediaStream | null>(null);
  const [mode, setMode] = useState<"prompt" | "live" | "fallback">("prompt");
  const [busy, setBusy] = useState(false);
  const [progress, setProgress] = useState(0);

  const stop = () => {
    streamRef.current?.getTracks().forEach((track) => track.stop());
    streamRef.current = null;
  };

  // Release the camera if the component unmounts mid-session.
  useEffect(() => stop, []);

  useEffect(() => {
    if (mode === "live" && videoRef.current && streamRef.current) {
      videoRef.current.srcObject = streamRef.current;
      void videoRef.current.play().catch(() => undefined);
    }
  }, [mode]);

  const start = async () => {
    onError("");
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

  const shutter = () => {
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
    canvas.toBlob(
      (blob) => {
        if (!blob) {
          setBusy(false);
          onError("Could not capture the frame.");
          return;
        }
        ocrTicket(blob, setProgress)
          .then(({ text, prepared }) => {
            stop();
            onResult(text, URL.createObjectURL(prepared));
          })
          .catch((e) => {
            onError(message(e));
            setBusy(false);
          });
      },
      "image/jpeg",
      0.95,
    );
  };

  if (mode === "fallback") return <PhotoCapture onResult={onResult} onError={onError} />;

  if (mode === "prompt") {
    return (
      <section className="step">
        <p>Scan the ticket with the camera, or use an existing photo. Fields are extracted then you confirm.</p>
        <div className="actions">
          <button type="button" onClick={start}>
            📷 Enable camera
          </button>
          <button type="button" className="link" onClick={() => setMode("fallback")}>
            Use a photo instead
          </button>
        </div>
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
            <button
              type="button"
              className="link"
              onClick={() => {
                stop();
                setMode("prompt");
              }}
            >
              Cancel
            </button>
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
      const { text, prepared } = await ocrTicket(file, setProgress);
      onResult(text, URL.createObjectURL(prepared));
    } catch (e) {
      onError(message(e));
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <p>Pick a photo of the ticket. Fields are extracted then you confirm.</p>
      <label className="capture-button">
        {busy ? `Reading… ${Math.round(progress * 100)}%` : "🖼️ Choose a photo"}
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
