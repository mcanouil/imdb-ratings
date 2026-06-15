import { stripDiacritics } from "./util";

export interface ImdbSuggestion {
  id: string; // tt-prefixed
  title: string;
  year?: number;
  type?: string; // "feature", "TV series", ...
  cast?: string;
  poster?: string;
}

interface RawSuggestion {
  id: string;
  l: string;
  y?: number;
  q?: string;
  s?: string;
  i?: [string, number, number] | { imageUrl: string };
}

const JSONP_TIMEOUT_MS = 8000;

/**
 * Build a callback-safe query for the legacy JSONP endpoint.
 * The endpoint keeps only the FIRST separator as "_" (later ones become "%20",
 * an invalid JS identifier), so cap at two words joined by a single underscore.
 * IMDb's fuzzy match still surfaces longer / localised titles.
 */
export function buildQuery(title: string): string {
  return stripDiacritics(title)
    .toLowerCase()
    .replace(/[^a-z0-9 ]+/g, " ")
    .trim()
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .join("_");
}

function poster(i: RawSuggestion["i"]): string | undefined {
  if (!i) return undefined;
  return Array.isArray(i) ? i[0] : i.imageUrl;
}

/** Search IMDb title suggestions via CORS-immune JSONP (`<script>` injection). */
export function searchImdb(title: string): Promise<ImdbSuggestion[]> {
  const query = buildQuery(title);
  if (!query) return Promise.resolve([]);

  const callbackName = `imdb$${query}`;
  const url = `https://sg.media-imdb.com/suggests/${query[0]}/${query}.json`;

  return new Promise((resolve, reject) => {
    const script = document.createElement("script");
    let timer = 0;

    const cleanup = () => {
      window.clearTimeout(timer);
      delete (window as unknown as Record<string, unknown>)[callbackName];
      script.remove();
    };

    (window as unknown as Record<string, unknown>)[callbackName] = (payload: { d?: RawSuggestion[] }) => {
      cleanup();
      const items = (payload.d ?? [])
        .filter((r) => typeof r.id === "string" && r.id.startsWith("tt"))
        .map((r) => ({ id: r.id, title: r.l, year: r.y, type: r.q, cast: r.s, poster: poster(r.i) }));
      resolve(items);
    };

    script.onerror = () => {
      cleanup();
      reject(new Error("IMDb suggestion request failed"));
    };

    timer = window.setTimeout(() => {
      cleanup();
      reject(new Error("IMDb suggestion request timed out"));
    }, JSONP_TIMEOUT_MS);

    script.src = url;
    document.head.appendChild(script);
  });
}
