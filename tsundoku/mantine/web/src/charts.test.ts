// The charts get the shapes they want, from the API's numbers.
// グラフは、API の数から、欲しい形を受け取る
import { describe, expect, it } from "vitest";
import { cloudSize, kindSlices, monthRows } from "./charts";

describe("monthRows", () => {
  // 「12か月そろえて、送られなかった月は 0」
  it("fills the twelve months, with 0 where nothing was read", () => {
    const rows = monthRows(2026,
      [{ month: "2026-11", finished: 1, pages: 78 }]);
    expect(rows).toHaveLength(12);
    expect(rows[10]).toEqual({ month: "11月", pages: 78, finished: 1 });
    expect(rows[0]).toEqual({ month: "1月", pages: 0, finished: 0 });
  });
  // 「ほかの年の月は数えない」
  it("leaves out the months of another year", () => {
    const rows = monthRows(2026,
      [{ month: "2025-11", finished: 1, pages: 9 }]);
    expect(rows.every((r) => r.pages === 0 && r.finished === 0)).toBe(true);
  });
});

describe("kindSlices", () => {
  // 「種類の言葉を画面の言葉に。知らない種類は、そのまま灰色で」
  it("names the kinds in the screen's words", () => {
    const counts = [{ kind: "book", works: 2 }, { kind: "zine", works: 1 }];
    expect(kindSlices(counts))
      .toEqual([{ name: "本", value: 2, color: "indigo.6" },
                { name: "zine", value: 1, color: "gray.6" }]);
  });
});

describe("cloudSize", () => {
  const cloud = [{ tag: "魔法", works: 5 }, { tag: "竜", works: 1 }];
  // 「いちばん多いタグは xl、少ないほど小さい」
  it("makes the most worn tag xl, and fewer smaller", () => {
    expect(cloudSize(5, cloud)).toBe("xl");
    expect(cloudSize(3, cloud)).toBe("md");
    expect(cloudSize(1, cloud)).toBe("xs");
  });
  // 「0 作品でも、xs より小さくはならない」
  it("never goes below xs", () => {
    expect(cloudSize(0, cloud)).toBe("xs");
    expect(cloudSize(0, [])).toBe("xs");
  });
});
