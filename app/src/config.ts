// Auth gateway (Netlify Functions). See dev-plan Component 4 / Step B.
export const GATEWAY_BASE = "https://api.canouil.dev";

// Target of the commit.
export const REPO_OWNER = "mcanouil";
export const REPO_NAME = "imdb-ratings";
export const REPO_BRANCH = "main";
export const CSV_PATH = "data/theatres.csv";

// Known theatre codes used in data/theatres.csv. Editable in the UI; not frozen.
export const THEATRE_CODES = ["LILLE", "MAJESTIC", "METROPOLE", "KINEPOLIS", "VDA"] as const;
