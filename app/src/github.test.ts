import { describe, it, expect } from "vitest";
import { buildRow } from "./github";
import type { TicketFields } from "./parseTicket";

const FIELDS: TicketFields = {
  theatre: "MAJESTIC",
  room: "1",
  date: "2026-06-13",
  time: "14:00",
  title: "x",
};

describe("buildRow", () => {
  it("formats date_time,room,theatre,imdb_id", () => {
    expect(buildRow(FIELDS, "tt42575634")).toBe("2026-06-13 14:00,1,MAJESTIC,tt42575634");
  });

  it("strips a comma injected via imdbId so no extra column appears", () => {
    const row = buildRow(FIELDS, "tt42575634,evil");
    expect(row.split(",")).toHaveLength(4);
    expect(row.endsWith(",tt42575634")).toBe(true);
  });

  it("strips a newline injected via imdbId so no extra row appears", () => {
    const row = buildRow(FIELDS, "tt42575634\n2000-01-01 00:00,9,EVIL,tt9999999");
    expect(row.split("\n")).toHaveLength(1);
    expect(row).toBe("2026-06-13 14:00,1,MAJESTIC,tt42575634");
  });

  it("drops an imdbId that is not tt-prefixed digits", () => {
    expect(buildRow(FIELDS, "notanid")).toBe("2026-06-13 14:00,1,MAJESTIC,");
  });

  it("neutralises commas and newlines smuggled through date/time/theatre", () => {
    const dirty: TicketFields = {
      theatre: "MAJESTIC,EVIL",
      room: "1",
      date: "2026-06-13,x",
      time: "14:00\n9,9",
      title: "",
    };
    const row = buildRow(dirty, "tt42575634");
    expect(row.split("\n")).toHaveLength(1);
    expect(row.split(",")).toHaveLength(4);
  });
});
