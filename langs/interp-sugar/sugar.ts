// The TypeScript code in the chapter on patterns and sugar.
// パターンと糖衣構文の章の TypeScript のコード
const scores: [string, number][] = [
  ["Python", 82],
  ["TS", 75],
  ["Rust", 91],
  ["Go", 68],
];

// ANCHOR: destructure
// TypeScript: take each pair apart in the arguments, then chain methods
// TypeScript：引数で組をほどき、メソッドをつなぐ
const passed = scores
  .filter(([, score]) => score >= 70)
  .map(([name]) => name);

const total = scores
  .map(([, score]) => score)
  .reduce((a, b) => a + b, 0);

const pairs = scores.flatMap(([a]) =>
  scores.filter(([b]) => a < b).map(([b]) => [a, b]),
);
// ANCHOR_END: destructure

if (passed.join(",") !== "Python,TS,Rust") throw new Error("passed");
if (total !== 316) throw new Error("total");
if (pairs.length !== 6) throw new Error("pairs");
