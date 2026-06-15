import { describe, it, expect } from "vitest";
import { buildRow } from "./github";

describe("buildRow", () => {
  it("formats date_time,room,theatre,imdb_id", () => {
    expect(buildRow({ theatre: "MAJESTIC", room: "1", date: "2026-06-13", time: "14:00", title: "x" }, "tt42575634")).toBe(
      "2026-06-13 14:00,1,MAJESTIC,tt42575634",
    );
  });
});
