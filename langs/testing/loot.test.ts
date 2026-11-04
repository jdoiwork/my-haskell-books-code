// The fast-check code in the testing chapter.
// テストの章の fast-check のコード
// ANCHOR: property_tests_with_fast_check
// TypeScript (property tests with fast-check)
// TypeScript（fast-check で性質のテスト）
import fc from "fast-check";
import { test } from "vitest";

test("keeps the total", () => {
  fc.assert(
    fc.property(
      fc.nat(),
      fc.integer({ min: 1, max: 100 }),
      (gold, members) => sum(splitLoot(gold, members)) === gold,
    ),
  );
});
// ANCHOR_END: property_tests_with_fast_check
function splitLoot(gold: number, members: number): number[] {
  const share = Math.floor(gold / members);
  const rest = gold % members;
  return Array.from({ length: members }, (_, i) => (i < rest ? share + 1 : share));
}

function sum(xs: number[]): number {
  return xs.reduce((acc, x) => acc + x, 0);
}
