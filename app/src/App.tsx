import { useState } from "react";
import { THEATRE_CODES } from "./config";
import { ocrTicket } from "./ocr";
import { parseTicket, type TicketFields } from "./parseTicket";
import { searchImdb, type ImdbSuggestion } from "./imdb";
import { buildRow, commit, type CommitResult } from "./github";
import { REPO_URL } from "./config";
import { clearToken, getStoredToken, pollForToken, startDeviceFlow, type DeviceCode } from "./auth";

type Step = "capture" | "confirm" | "pick" | "review" | "done";

const EMPTY: TicketFields = { theatre: "", room: "", date: "", time: "", title: "" };

export function App() {
  const [step, setStep] = useState<Step>("capture");
  const [fields, setFields] = useState<TicketFields>(EMPTY);
  const [imageUrl, setImageUrl] = useState("");
  const [suggestions, setSuggestions] = useState<ImdbSuggestion[]>([]);
  const [imdbId, setImdbId] = useState("");
  const [committed, setCommitted] = useState<{ row: string; result: CommitResult } | null>(null);
  const [error, setError] = useState("");

  const reset = () => {
    setStep("capture");
    setFields(EMPTY);
    setImageUrl("");
    setSuggestions([]);
    setImdbId("");
    setCommitted(null);
    setError("");
  };

  return (
    <main className="app">
      <header>
        <h1>🎟️ Ticket → theatres.csv</h1>
        <Stepper step={step} />
      </header>

      {error && (
        <p className="error" role="alert">
          {error}
        </p>
      )}

      {step === "capture" && (
        <CaptureStep
          onError={setError}
          onResult={(text, url) => {
            setFields(parseTicket(text));
            setImageUrl(url);
            setError("");
            setStep("confirm");
          }}
        />
      )}

      {step === "confirm" && (
        <ConfirmStep
          fields={fields}
          imageUrl={imageUrl}
          onChange={setFields}
          onBack={reset}
          onSearch={async () => {
            setError("");
            try {
              const found = await searchImdb(fields.title);
              setSuggestions(found);
              setStep("pick");
            } catch (e) {
              setError(asMessage(e));
            }
          }}
        />
      )}

      {step === "pick" && (
        <PickStep
          initialQuery={fields.title}
          suggestions={suggestions}
          onResearch={setSuggestions}
          onError={setError}
          onBack={() => setStep("confirm")}
          onSelect={(id) => {
            setImdbId(id);
            setStep("review");
          }}
        />
      )}

      {step === "review" && (
        <ReviewStep
          row={buildRow(fields, imdbId)}
          onBack={() => setStep("pick")}
          onError={setError}
          onCommitted={(row, result) => {
            setCommitted({ row, result });
            setStep("done");
          }}
        />
      )}

      {step === "done" && committed && <DoneStep row={committed.row} result={committed.result} onReset={reset} />}
    </main>
  );
}

function Stepper({ step }: { step: Step }) {
  const order: Step[] = ["capture", "confirm", "pick", "review", "done"];
  const labels: Record<Step, string> = {
    capture: "Scan",
    confirm: "Confirm",
    pick: "Film",
    review: "Commit",
    done: "Done",
  };
  const current = order.indexOf(step);
  return (
    <ol className="stepper">
      {order.map((s, i) => (
        <li key={s} className={i === current ? "active" : i < current ? "past" : ""}>
          {labels[s]}
        </li>
      ))}
    </ol>
  );
}

function CaptureStep({ onResult, onError }: { onResult: (text: string, url: string) => void; onError: (m: string) => void }) {
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
      onError(asMessage(e));
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <p>Photograph the cinema ticket, or pick an existing photo. Fields are extracted then you confirm.</p>
      <label className="capture-button">
        {busy ? `Reading… ${Math.round(progress * 100)}%` : "📷 Scan ticket"}
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

function ConfirmStep({
  fields,
  imageUrl,
  onChange,
  onSearch,
  onBack,
}: {
  fields: TicketFields;
  imageUrl: string;
  onChange: (f: TicketFields) => void;
  onSearch: () => void;
  onBack: () => void;
}) {
  const set = (key: keyof TicketFields) => (e: { target: { value: string } }) => onChange({ ...fields, [key]: e.target.value });
  const ready = fields.theatre && fields.room && fields.date && fields.time && fields.title;

  return (
    <section className="step">
      {imageUrl && (
        <figure className="preview-figure">
          <img className="preview" src={imageUrl} alt="Binarised ticket used for OCR" />
          <figcaption>Binarised image fed to OCR. Correct any misread field below.</figcaption>
        </figure>
      )}
      <form
        onSubmit={(e) => {
          e.preventDefault();
          if (ready) onSearch();
        }}
      >
        <label>
          Theatre
          <input list="theatres" value={fields.theatre} onChange={set("theatre")} />
          <datalist id="theatres">
            {THEATRE_CODES.map((c) => (
              <option key={c} value={c} />
            ))}
          </datalist>
        </label>
        <label>
          Room
          <input type="number" min="1" value={fields.room} onChange={set("room")} />
        </label>
        <label>
          Date
          <input type="date" value={fields.date} onChange={set("date")} />
        </label>
        <label>
          Time
          <input type="time" value={fields.time} onChange={set("time")} />
        </label>
        <label>
          Title
          <input type="text" value={fields.title} onChange={set("title")} placeholder="Film title" />
        </label>
        <div className="actions">
          <button type="button" className="secondary" onClick={onBack}>
            Rescan
          </button>
          <button type="submit" disabled={!ready}>
            Search IMDb →
          </button>
        </div>
      </form>
    </section>
  );
}

function PickStep({
  initialQuery,
  suggestions,
  onResearch,
  onSelect,
  onError,
  onBack,
}: {
  initialQuery: string;
  suggestions: ImdbSuggestion[];
  onResearch: (s: ImdbSuggestion[]) => void;
  onSelect: (id: string) => void;
  onError: (m: string) => void;
  onBack: () => void;
}) {
  const [query, setQuery] = useState(initialQuery);
  const [busy, setBusy] = useState(false);

  const research = async () => {
    setBusy(true);
    onError("");
    try {
      onResearch(await searchImdb(query));
    } catch (e) {
      onError(asMessage(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <div className="research">
        <input type="text" value={query} onChange={(e) => setQuery(e.target.value)} placeholder="Refine title" />
        <button type="button" onClick={research} disabled={busy || !query.trim()}>
          {busy ? "…" : "Search"}
        </button>
      </div>
      {suggestions.length === 0 && <p>No matches. Refine the title and search again.</p>}
      <ul className="results">
        {suggestions.map((s) => (
          <li key={s.id}>
            <button type="button" onClick={() => onSelect(s.id)}>
              {s.poster ? <img src={s.poster} alt="" loading="lazy" /> : <span className="no-poster" />}
              <span className="meta">
                <strong>{s.title}</strong>
                <span>
                  {s.year ?? "—"} · {s.type ?? "title"} · {s.id}
                </span>
                {s.cast && <span className="cast">{s.cast}</span>}
              </span>
            </button>
          </li>
        ))}
      </ul>
      <div className="actions">
        <button type="button" className="secondary" onClick={onBack}>
          ← Back
        </button>
      </div>
    </section>
  );
}

function ReviewStep({
  row,
  onCommitted,
  onError,
  onBack,
}: {
  row: string;
  onCommitted: (row: string, result: CommitResult) => void;
  onError: (m: string) => void;
  onBack: () => void;
}) {
  const [token, setToken] = useState(getStoredToken());
  const [device, setDevice] = useState<DeviceCode | null>(null);
  const [busy, setBusy] = useState(false);

  const login = async () => {
    setBusy(true);
    onError("");
    try {
      const dev = await startDeviceFlow();
      setDevice(dev);
      const t = await pollForToken(dev);
      setToken(t);
      setDevice(null);
    } catch (e) {
      onError(asMessage(e));
      setDevice(null);
    } finally {
      setBusy(false);
    }
  };

  const doCommit = async () => {
    if (!token) return;
    setBusy(true);
    onError("");
    try {
      const result = await commit(token, row);
      onCommitted(row, result);
    } catch (e) {
      onError(asMessage(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <p>New row to commit:</p>
      <pre className="row">{row}</pre>

      {!token && !device && (
        <button type="button" onClick={login} disabled={busy}>
          🔐 Sign in with GitHub
        </button>
      )}

      {device && (
        <div className="device">
          <p>
            Open{" "}
            <a href={device.verification_uri} target="_blank" rel="noreferrer">
              {device.verification_uri}
            </a>{" "}
            and enter this code:
          </p>
          <p className="user-code">{device.user_code}</p>
          <p className="waiting">Waiting for authorisation…</p>
        </div>
      )}

      {token && (
        <div className="actions">
          <button type="button" className="secondary" onClick={onBack}>
            ← Back
          </button>
          <button type="button" onClick={doCommit} disabled={busy}>
            {busy ? "Committing…" : "✅ Commit to theatres.csv"}
          </button>
          <button
            type="button"
            className="link"
            onClick={() => {
              clearToken();
              setToken(null);
            }}
          >
            Logout
          </button>
        </div>
      )}
    </section>
  );
}

function DoneStep({ row, result, onReset }: { row: string; result: CommitResult; onReset: () => void }) {
  return (
    <section className="step done">
      <p>✅ Committed. The render workflow will refresh the figures shortly.</p>
      <pre className="row">{row}</pre>
      <p>{result.verified ? "🔒 Signed commit (Verified)." : "⚠️ Commit not verified."}</p>
      <p>
        <a href={result.html_url ?? `${REPO_URL}/commits/main`} target="_blank" rel="noreferrer">
          View commit
        </a>
      </p>
      <button type="button" onClick={onReset}>
        Scan another
      </button>
    </section>
  );
}

function asMessage(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}
