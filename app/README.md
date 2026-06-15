# Ticket scanner

Mobile web app that scans a cinema ticket, resolves the film to an IMDb id, and commits a new row to `data/theatres.csv`.

## Develop

```bash
cd app
npm install
npm run dev        # local dev server
npm test           # unit tests (parse / IMDb query / CSV insert)
npm run build      # type-check + production build to dist/
```

## How it works

- **OCR**: Tesseract.js (`fra`) reads the ticket photo client-side (`src/ocr.ts`).
- **Parse**: `src/parseTicket.ts` extracts theatre / room / date / time / title; every field is editable before submit.
- **IMDb**: `src/imdb.ts` queries the legacy JSONP suggestion endpoint (CORS-immune via `<script>`), capped at two words so the callback stays valid.
- **Auth**: GitHub App device flow via the Netlify gateway at `api.canouil.dev` (`src/auth.ts`); token kept in `sessionStorage`.
- **Commit**: `src/github.ts` inserts the row in descending `date_time` order and PUTs it to `main`, which triggers the existing `render.yml`.

## Configuration

Endpoints and repo target live in `src/config.ts`.

The one-time manual setup (GitHub App, Netlify gateway + OVH CNAME, Pages) is documented in
`.claude/dev-plans/design-a-movie-web-cheeky-sundae.md`.
