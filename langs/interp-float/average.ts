// The TypeScript code in the chapter on floats.
// 小数の章の TypeScript のコード

const scores = [82, 75, 91, 64];

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function average(xs: number[]): number {
  let total = 0;
  for (const x of xs) {
    total += x;
  }
  return total / xs.length;    // one type, number: 78
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const average2 = (xs: number[]): number =>
  xs.reduce((a, b) => a + b, 0) / xs.length;

const wholeAverage = (xs: number[]): number =>
  Math.floor(average2(xs));    // no // : round it yourself
// ANCHOR_END: functional

// ANCHOR: surprises
// TypeScript (a few surprises)
// TypeScript（ちょっとした驚き）
const halves = [Math.round(2.5), Math.round(-2.5)];  // [3, -2]
const infinite = 1 / 0;                              // Infinity
const big = -7n / 2n;          // BigInt: -3n, toward 0
// ANCHOR_END: surprises

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(what);
}
check(average(scores) === 78 && average2(scores) === 78, "average");
check(wholeAverage([7, 8]) === 7, "whole");
check(halves[0] === 3 && halves[1] === -2, "halves");
check(infinite === Infinity && Number.isNaN(0 / 0), "infinite");
check(big === -3n, "bigint");
check(String(0.1 + 0.2) === "0.30000000000000004", "point");
console.log(average(scores), wholeAverage([7, 8]), halves, big);
