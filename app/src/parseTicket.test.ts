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

const BACKROOMS = `CINE CITE LILLE        Salle 06 F11
dim. 21 juin 2026        10:45
BACKROOMS VOstF
Abonnement UGC Illimité          AVE -12
UI ******244103-
21/06/2026 10:55:18.752
3-017-335226972-6-0150295
LILLE-2-42(42)
Op.4200000049198653
UGC ILLIMITE
Ni repris, ni echange`;

describe("parseTicket", () => {
  it("extracts every field from the Backrooms ticket with an inverted room box", () => {
    expect(parseTicket(BACKROOMS)).toEqual({
      theatre: "LILLE",
      room: "6",
      date: "2026-06-21",
      time: "10:45",
      title: "BACKROOMS",
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

  it("ignores an adjacent seat code", () => {
    expect(parseRoom("Salle 06 F11")).toBe("6");
    expect(parseRoom("Salle 06F11")).toBe("6");
  });

  it("reads O as a misread zero", () => {
    expect(parseRoom("Salle O6")).toBe("6");
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
