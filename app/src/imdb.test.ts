import { describe, it, expect } from "vitest";
import { buildQuery } from "./imdb";

describe("buildQuery", () => {
  it("drops the version tag and joins at most two words with a single underscore", () => {
    expect(buildQuery("LE VERTIGE VF")).toBe("le_vertige");
  });

  it("caps long titles at two words so the JSONP callback stays valid", () => {
    expect(buildQuery("28 ans plus tard")).toBe("28_ans");
    expect(buildQuery("Le Seigneur des Anneaux")).toBe("le_seigneur");
  });

  it("strips diacritics and punctuation", () => {
    expect(buildQuery("Amélie")).toBe("amelie");
    expect(buildQuery("Mission: Impossible")).toBe("mission_impossible");
  });

  it("returns empty for blank input", () => {
    expect(buildQuery("   ")).toBe("");
  });
});
