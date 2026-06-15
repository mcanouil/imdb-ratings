/// <reference types="vitest/config" />
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// Served from the GitHub Pages project path https://<user>.github.io/imdb-ratings/.
export default defineConfig({
  base: "/imdb-ratings/",
  plugins: [react()],
  test: {
    environment: "node",
    include: ["src/**/*.test.ts"],
  },
});
