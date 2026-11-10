// The rows of the star chart.
// ★のグラフの行
import { describe, expect, it } from "vitest";
import { starRows } from "./StarChart";

describe("starRows", () => {
  // 「目盛りごとに1行、評価のない作品は数えない」
  it("makes a row for each mark, leaving out the unrated", () => {
    const rows = starRows([90, 88, 73, null]);
    expect(rows).toHaveLength(11);
    expect(rows[0]).toEqual({ mark: "★0", works: 0 });
    expect(rows.find((r) => r.mark === "★4.5"))
      .toEqual({ mark: "★4.5", works: 2 });
    expect(rows.reduce((n, r) => n + r.works, 0)).toBe(3);
  });
});
