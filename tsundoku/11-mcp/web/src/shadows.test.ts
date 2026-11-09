// The shadows say what the Haskell side says.
// 影は、Haskell の側と同じことを言う
import { describe, expect, it } from "vitest";
import { eventLabel, marks, stars, stateLabel, whereOf } from "./shadows";

describe("stars", () => {
  // 「Rating.render FiveStars と同じ丸め」
  it("rounds like Rating.render FiveStars", () => {
    expect(stars(90)).toBe("★4.5");
    expect(stars(73)).toBe("★3.5");
    expect(stars(75)).toBe("★4");
    expect(stars(0)).toBe("★0");
    expect(stars(100)).toBe("★5");
    expect(stars(null)).toBe("—");
  });
  // 「目盛りは 0 から 100 まで 10 ずつ」
  it("has eleven marks", () => {
    expect(marks).toEqual([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]);
  });
});

describe("labels", () => {
  // 「知らない言葉は、そのまま」
  it("says the words, and keeps unknown names as they are", () => {
    expect(stateLabel("Reading")).toBe("読書中");
    expect(eventLabel("Finish")).toBe("読み終えた");
    expect(stateLabel("Flying")).toBe("Flying");
  });
});

describe("whereOf", () => {
  const toc = [
    { id: 1, heading: "第1章", size: 4, done: false, rating: null,
      children: [{ id: 2, heading: "1.1 うろこの色", size: 8, done: false,
                   rating: null, children: [] }] },
  ];
  // 「子の見出しも見つける。なければ作品の全体か、目次にない項目」
  it("finds the heading, even of a child", () => {
    expect(whereOf(toc, 2)).toBe("1.1 うろこの色");
    expect(whereOf(toc, null)).toBe("作品の全体");
    expect(whereOf(toc, 9)).toBe("（目次にない項目）");
  });
});
