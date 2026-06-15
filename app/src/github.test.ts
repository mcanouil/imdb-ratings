import { describe, it, expect } from "vitest";
import { buildRow, insertRow } from "./github";

const CSV = `date_time,room,theatre,imdb_id
2026-06-13 14:00,1,MAJESTIC,tt42575634
2026-06-11 17:45,1,LILLE,tt15047880
2026-06-07 10:45,3,LILLE,tt32093575
`;

describe("buildRow", () => {
  it("formats date_time,room,theatre,imdb_id", () => {
    expect(buildRow({ theatre: "MAJESTIC", room: "1", date: "2026-06-13", time: "14:00", title: "x" }, "tt42575634")).toBe(
      "2026-06-13 14:00,1,MAJESTIC,tt42575634",
    );
  });
});

describe("insertRow", () => {
  it("inserts a middle date in descending position", () => {
    const out = insertRow(CSV, "2026-06-09 20:00,2,LILLE,tt99999999");
    expect(out.split("\n")[3]).toBe("2026-06-09 20:00,2,LILLE,tt99999999");
  });

  it("inserts the newest date right after the header", () => {
    const out = insertRow(CSV, "2026-06-20 10:00,4,VDA,tt11111111");
    expect(out.split("\n")[1]).toBe("2026-06-20 10:00,4,VDA,tt11111111");
  });

  it("appends the oldest date at the end and preserves the trailing newline", () => {
    const out = insertRow(CSV, "2020-01-01 09:00,5,LILLE,tt22222222");
    const lines = out.split("\n");
    expect(out.endsWith("\n")).toBe(true);
    expect(lines[lines.length - 2]).toBe("2020-01-01 09:00,5,LILLE,tt22222222");
  });

  it("keeps the header untouched", () => {
    const out = insertRow(CSV, "2026-06-20 10:00,4,VDA,tt11111111");
    expect(out.split("\n")[0]).toBe("date_time,room,theatre,imdb_id");
  });
});
