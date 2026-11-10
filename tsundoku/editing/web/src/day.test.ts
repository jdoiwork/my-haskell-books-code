// ANCHOR: all
// 🧪 The day sent with a mark.
// 読んだ印と一緒に送る日付
import { describe, expect, it } from "vitest";

import { dayOf } from "./day";

describe("dayOf", () => {
  // 「月と日は2桁にそろえる」
  it("writes the month and the day with two digits", () => {
    expect(dayOf(new Date(2026, 0, 5, 12))).toBe("2026-01-05");
  });
  // 「真夜中の少しあとも、その日のうち（UTC の日付ではない）」
  it("keeps a moment after midnight on its own day", () => {
    expect(dayOf(new Date(2026, 10, 10, 0, 30))).toBe("2026-11-10");
  });
});
// ANCHOR_END: all
