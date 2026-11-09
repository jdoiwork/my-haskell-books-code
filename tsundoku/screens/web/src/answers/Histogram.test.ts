// The spread counts each rated work once, on its rounded mark.
// 分布は、評価のある作品を1回ずつ、丸めた目盛りに数える
import { expect, it } from "vitest";
import { histogram } from "./Histogram";

// 「目盛りに丸めて数え、評価のない作品は数えない」
it("counts each rated work on its mark, and skips the unrated", () => {
  const counts = histogram([90, 92, 73, null, 100]);
  expect(counts.get(90)).toBe(2);
  expect(counts.get(70)).toBe(1);
  expect(counts.get(100)).toBe(1);
  expect([...counts.values()].reduce((a, b) => a + b, 0)).toBe(4);
  expect([...counts.keys()])
    .toEqual([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]);
});
