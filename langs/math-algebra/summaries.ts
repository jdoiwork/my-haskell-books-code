// Combinable summaries of the chapter "Associativity and monoids",
// in TypeScript.
// 「結合法則とモノイド」の章の、合わせられるまとめを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const servers = [[10, 20], [30, 400], [15]];

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
type Summary = { count: number; total: number };
const empty: Summary = { count: 0, total: 0 };
const combine = (a: Summary, b: Summary): Summary => ({
  count: a.count + b.count,
  total: a.total + b.total,
});
const summarize = (log: number[]): Summary => ({
  count: log.length,
  total: log.reduce((x, y) => x + y, 0),
});
const all = servers.map(summarize).reduce(combine, empty);
// ANCHOR_END: functional

check(all.total / all.count === 95, "mean");
check([].reduce(combine, empty).count === 0, "empty");
console.log("ok");
