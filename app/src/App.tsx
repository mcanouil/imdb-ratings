import { useEffect, useState, type ReactNode } from "react";
import { OWNER_LOGIN, REPO_URL, THEATRE_CODES } from "./config";
import { CameraCapture } from "./CameraCapture";
import { parseTicket, type TicketFields } from "./parseTicket";
import { searchImdb, type ImdbSuggestion } from "./imdb";
import { buildRow, commit, type CommitResult } from "./github";
import { clearToken, getValidToken, isOwner, pollForToken, startDeviceFlow, type DeviceCode } from "./auth";
import { usePullToRefresh } from "./usePullToRefresh";
import { errorMessage } from "./util";

type Step = "capture" | "confirm" | "pick" | "review" | "done";

const EMPTY: TicketFields = { theatre: "", room: "", date: "", time: "", title: "" };

export function App() {
  const { distance, threshold, refreshing } = usePullToRefresh();
  return (
    <>
      <PullIndicator distance={distance} threshold={threshold} refreshing={refreshing} />
      <AuthGate>{(token, logout) => <Scanner token={token} onLogout={logout} />}</AuthGate>
    </>
  );
}

function PullIndicator({ distance, threshold, refreshing }: { distance: number; threshold: number; refreshing: boolean }) {
  if (distance === 0 && !refreshing) return null;
  const ready = refreshing || distance >= threshold;
  return (
    <div className="ptr" style={{ transform: `translateY(${refreshing ? threshold : distance}px)` }} aria-hidden="true">
      <span className={`ptr-spinner${refreshing ? " spin" : ""}`} style={{ transform: `rotate(${distance * 3}deg)` }}>
        ↻
      </span>
      <span className="ptr-label">{refreshing ? "Refreshing…" : ready ? "Release to refresh" : "Pull to refresh"}</span>
    </div>
  );
}

/** Front door: nothing renders until a device-flow login resolves to the repo owner. */
function AuthGate({ children }: { children: (token: string, logout: () => void) => ReactNode }) {
  const [token, setToken] = useState<string | null>(null);
  const [status, setStatus] = useState<"checking" | "locked" | "authed">("checking");
  const [device, setDevice] = useState<DeviceCode | null>(null);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState("");

  // Verify a token belongs to the owner, then either unlock or reject (used by both paths).
  const acceptToken = async (t: string): Promise<void> => {
    const ok = await isOwner(t).catch(() => false);
    if (ok) {
      setToken(t);
      setStatus("authed");
    } else {
      clearToken();
      setStatus("locked");
      setError("This account is not authorised.");
    }
  };

  // On load: restore (and refresh if needed) the persisted session, then verify ownership.
  useEffect(() => {
    let cancelled = false;
    (async () => {
      const stored = await getValidToken();
      if (cancelled) return;
      if (stored) await acceptToken(stored);
      else setStatus("locked");
    })();
    return () => {
      cancelled = true;
    };
  }, []);

  const login = async () => {
    setBusy(true);
    setError("");
    try {
      const dev = await startDeviceFlow();
      setDevice(dev);
      const t = await pollForToken(dev);
      setDevice(null);
      await acceptToken(t);
    } catch (e) {
      setError(errorMessage(e));
      setDevice(null);
    } finally {
      setBusy(false);
    }
  };

  const logout = () => {
    clearToken();
    setToken(null);
    setStatus("locked");
    setError("");
  };

  if (status === "authed" && token) return children(token, logout);

  return (
    <main className="app">
      <header>
        <h1>🎟️ Ticket → theatres.csv</h1>
      </header>
      <section className="step gate">
        {status === "checking" ? (
          <p>Checking session…</p>
        ) : (
          <>
            <p>Restricted to {OWNER_LOGIN}. Sign in with GitHub to continue.</p>
            {error && (
              <p className="error" role="alert">
                {error}
              </p>
            )}
            {!device && (
              <button type="button" onClick={login} disabled={busy}>
                {busy ? "…" : "🔐 Sign in with GitHub"}
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
          </>
        )}
      </section>
    </main>
  );
}

function Scanner({ token, onLogout }: { token: string; onLogout: () => void }) {
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

  // Release each preview object URL when it is replaced or the scanner unmounts.
  useEffect(() => {
    if (!imageUrl) return;
    return () => URL.revokeObjectURL(imageUrl);
  }, [imageUrl]);

  return (
    <main className="app">
      <header>
        <div className="topbar">
          <h1>🎟️ Ticket → theatres.csv</h1>
          <button type="button" className="link" onClick={onLogout}>
            Logout
          </button>
        </div>
        <Stepper step={step} />
      </header>

      {error && (
        <p className="error" role="alert">
          {error}
        </p>
      )}

      {step === "capture" && (
        <CameraCapture
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
              setError(errorMessage(e));
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
          token={token}
          row={buildRow(fields, imdbId)}
          onBack={() => setStep("pick")}
          onError={setError}
          onCommitted={(row, result) => {
            setCommitted({ row, result });
            setStep("done");
          }}
        />
      )}

      {step === "done" && committed && (
        <DoneStep row={committed.row} result={committed.result} imdbId={imdbId} onReset={reset} />
      )}
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
      onError(errorMessage(e));
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
  token,
  row,
  onCommitted,
  onError,
  onBack,
}: {
  token: string;
  row: string;
  onCommitted: (row: string, result: CommitResult) => void;
  onError: (m: string) => void;
  onBack: () => void;
}) {
  const [busy, setBusy] = useState(false);

  const doCommit = async () => {
    setBusy(true);
    onError("");
    try {
      const result = await commit(token, row);
      onCommitted(row, result);
    } catch (e) {
      onError(errorMessage(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <section className="step">
      <p>New row to commit:</p>
      <pre className="row">{row}</pre>
      <div className="actions">
        <button type="button" className="secondary" onClick={onBack}>
          ← Back
        </button>
        <button type="button" onClick={doCommit} disabled={busy}>
          {busy ? "Committing…" : "✅ Commit to theatres.csv"}
        </button>
      </div>
    </section>
  );
}

function DoneStep({
  row,
  result,
  imdbId,
  onReset,
}: {
  row: string;
  result: CommitResult;
  imdbId: string;
  onReset: () => void;
}) {
  return (
    <section className="step done">
      <p>✅ Committed. The render workflow will refresh the figures shortly.</p>
      <pre className="row">{row}</pre>
      <p>{result.verified ? "🔒 Signed commit (Verified)." : "⚠️ Commit not verified."}</p>
      <div className="actions">
        <a className="button" href={`https://www.imdb.com/title/${imdbId}/`} target="_blank" rel="noreferrer">
          ★ Rate on IMDb
        </a>
        <button type="button" onClick={onReset}>
          Scan another
        </button>
      </div>
      <p>
        <a href={result.html_url ?? `${REPO_URL}/commits/main`} target="_blank" rel="noreferrer">
          View commit
        </a>
      </p>
    </section>
  );
}

