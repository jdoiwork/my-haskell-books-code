// The shadows say what the Haskell side says.
// 影は、Haskell の側と同じことを言う
import { describe, expect, it } from "vitest";
import {
  eventLabel, halfStars, marks, pointsOf, pointsOfQuarter, quarterStars,
  stars, stateColor, stateLabel, whereOf,
} from "./shadows";

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

// ANCHOR: site
describe("halfStars", () => {
  // 「Rating の★と点を行き来する。丸め方は stars と同じ」
  it("goes between points and Rating's stars", () => {
    expect(halfStars(73)).toBe(3.5);
    expect(halfStars(75)).toBe(4);
    expect(pointsOf(3.5)).toBe(70);
    expect(marks.every((m) => pointsOf(halfStars(m)) === m)).toBe(true);
  });
});

describe("stateColor", () => {
  // 「状態ごとの色。知らない状態は灰色」
  it("colours each state, and gray for others", () => {
    expect(stateColor("Finished")).toBe("teal");
    expect(stateColor("Lost")).toBe("gray");
  });
});
// ANCHOR_END: site

// ANCHOR: quarter
describe("quarterStars", () => {
  // 「25 点ずつの照らし方。★1 が 0 点で、点のモデルは同じ」
  it("lights the same points 25 a star, from ★1 = 0", () => {
    expect([0, 25, 50, 75, 100].map(quarterStars)).toEqual([1, 2, 3, 4, 5]);
    expect(quarterStars(73)).toBe(4);
    expect(quarterStars(12)).toBe(1);
    expect(quarterStars(13)).toBe(2);
    expect([1, 2, 3, 4, 5].map(pointsOfQuarter)).toEqual([0, 25, 50, 75, 100]);
  });
});
// ANCHOR_END: quarter
