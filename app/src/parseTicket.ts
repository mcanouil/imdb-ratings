import { THEATRE_CODES } from "./config";
import { stripDiacritics } from "./util";

export interface TicketFields {
  /** Canonical theatre code, e.g. "MAJESTIC". Empty when undetected. */
  theatre: string;
  /** Room/screen number as a string for form binding. Empty when undetected. */
  room: string;
  /** ISO date "YYYY-MM-DD". Empty when undetected. */
  date: string;
  /** Showtime "HH:MM". Empty when undetected. */
  time: string;
  /** Film title used as the IMDb query. Empty when undetected. */
  title: string;
}

const MONTHS: Record<string, string> = {
  janvier: "01",
  fevrier: "02",
  mars: "03",
  avril: "04",
  mai: "05",
  juin: "06",
  juillet: "07",
  aout: "08",
  septembre: "09",
  octobre: "10",
  novembre: "11",
  decembre: "12",
};

// Lines that are never a film title.
const NOISE = [
  "abonnement",
  "ugc",
  "illimite",
  "salle",
  "ni repris",
  "ni echange",
  "echange",
  "carte",
  "place",
  "tarif",
];

// Trailing language / format tags printed after the title.
const VERSION_TAGS = /\s+(VF|VOST|VOSTFR|VOSTF|VO|3D|4DX|IMAX|ATMOS|ICE)\b.*$/i;

export function parseTheatre(text: string): string {
  const haystack = stripDiacritics(text).toUpperCase();
  // "LILLE" is the city name and appears on most tickets, so match specific
  // venue codes first and only fall back to LILLE.
  const ordered = [...THEATRE_CODES].sort((a, b) => Number(a === "LILLE") - Number(b === "LILLE"));
  for (const code of ordered) {
    if (haystack.includes(code)) return code;
  }
  return "";
}

export function parseRoom(text: string): string {
  // "Salle" may be followed by OCR noise (punctuation, the inverted box edge)
  // before the room number, which is printed as two digits. Tolerate `O` as a
  // misread `0` and cap the capture at two chars so an adjacent seat code
  // (e.g. "F11") cannot merge in.
  const match = text.match(/salle[\s.:|-]*([0-9oO]{1,2})/i);
  if (!match) return "";
  const room = Number(match[1].replace(/[oO]/g, "0"));
  return Number.isNaN(room) ? "" : String(room);
}

export function parseDate(text: string): string {
  const flat = stripDiacritics(text).toLowerCase();
  const match = flat.match(/(\d{1,2})\s+(janvier|fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre|novembre|decembre)\.?\s+(\d{4})/);
  if (!match) return "";
  const day = match[1].padStart(2, "0");
  const month = MONTHS[match[2]];
  const year = match[3];
  return `${year}-${month}-${day}`;
}

export function parseTime(text: string): string {
  // Collect HH:MM, ignoring HH:MM:SS print timestamps (those have seconds).
  const re = /\b([01]?\d|2[0-3])[:hH.]([0-5]\d)(?:[:.]\d{2})?\b/g;
  let best = "";
  for (let m = re.exec(text); m !== null; m = re.exec(text)) {
    const hasSeconds = /[:.]\d{2}$/.test(m[0].slice(5));
    if (hasSeconds) continue;
    best = `${m[1].padStart(2, "0")}:${m[2]}`;
    break;
  }
  return best;
}

export function parseTitle(text: string, theatre = ""): string {
  const lines = text
    .split(/\r?\n/)
    .map((l) => l.trim())
    .filter(Boolean);

  const candidates = lines.filter((line) => {
    const flat = stripDiacritics(line).toLowerCase();
    if (/\d/.test(line)) return false; // dates, codes, timestamps
    if (NOISE.some((n) => flat.includes(n))) return false;
    if (theatre && flat.includes(theatre.toLowerCase())) return false;
    const letters = line.replace(/[^A-Za-zÀ-ÿ]/g, "");
    if (letters.length < 3) return false;
    // Mostly uppercase line (ticket titles are stamped in caps).
    const upper = letters.replace(/[^A-ZÀ-Þ]/g, "");
    return upper.length / letters.length > 0.6;
  });

  const chosen = candidates.sort((a, b) => b.length - a.length)[0] ?? "";
  return chosen.replace(VERSION_TAGS, "").trim();
}

export function parseTicket(text: string): TicketFields {
  const theatre = parseTheatre(text);
  return {
    theatre,
    room: parseRoom(text),
    date: parseDate(text),
    time: parseTime(text),
    title: parseTitle(text, theatre),
  };
}
