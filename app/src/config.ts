// Auth + commit gateway (Netlify Functions). See dev-plan Component 4 / Step B.
export const GATEWAY_BASE = "https://api.canouil.dev";

// Repo the commit lands in (for display links only; the gateway owns the write).
export const REPO_URL = "https://github.com/mcanouil/imdb-ratings";

// Only this GitHub login may see the app and commit.
export const OWNER_LOGIN = "mcanouil";

// Known theatre codes used in data/theatres.csv. Editable in the UI; not frozen.
export const THEATRE_CODES = ["LILLE", "MAJESTIC", "METROPOLE", "KINEPOLIS", "VDA"] as const;
