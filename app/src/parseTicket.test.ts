import { describe, it, expect } from "vitest";
import { parseTicket, parseTheatre, parseRoom, parseDate, parseTime, parseTitle } from "./parseTicket";

const SAMPLE = `LE MAJESTIC LILLE        Salle 01 C13
sam. 13 juin 2026        14:00
LE VERTIGE VF
Abonnement UGC Illimité
UI ******244103
13/06/2026 14:11:38.315
3-017-335227181-1-0140498
MJTIC-2 40(40)
Op.650000007262684
UGC ILLIMITE
Ni repris, ni echange`;

describe("parseTicket", () => {
  it("extracts every field from the sample UGC ticket", () => {
    expect(parseTicket(SAMPLE)).toEqual({
      theatre: "MAJESTIC",
      room: "1",
      date: "2026-06-13",
      time: "14:00",
      title: "LE VERTIGE",
    });
  });
});

describe("parseTheatre", () => {
  it("prefers the venue code over the city name", () => {
    expect(parseTheatre("LE MAJESTIC LILLE")).toBe("MAJESTIC");
  });

  it("falls back to LILLE when no specific venue matches", () => {
    expect(parseTheatre("UGC CINE CITE LILLE")).toBe("LILLE");
  });

  it("returns empty when unknown", () => {
    expect(parseTheatre("PATHE GAUMONT")).toBe("");
  });
});

describe("parseRoom", () => {
  it("strips the leading zero", () => {
    expect(parseRoom("Salle 01")).toBe("1");
    expect(parseRoom("SALLE 13")).toBe("13");
  });
});

describe("parseDate", () => {
  it("maps the French month and zero-pads", () => {
    expect(parseDate("sam. 13 juin 2026")).toBe("2026-06-13");
    expect(parseDate("1 fevrier 2025")).toBe("2025-02-01");
    expect(parseDate("9 décembre 2024")).toBe("2024-12-09");
  });
});

describe("parseTime", () => {
  it("takes the showtime, not the print timestamp", () => {
    expect(parseTime(SAMPLE)).toBe("14:00");
  });
});

describe("parseTitle", () => {
  it("strips the trailing version tag", () => {
    expect(parseTitle(SAMPLE, "MAJESTIC")).toBe("LE VERTIGE");
  });
});
