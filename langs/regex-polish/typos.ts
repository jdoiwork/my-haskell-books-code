// TypeScript checks the syntax of a regex literal (a wrong one stops
// tsc: see check_literal.py), but the names of the groups are not in
// the type: groups is { [key: string]: string }, so a typo is
// undefined when it runs. Compare with [typed|…|] of regex-polish.md.
// TypeScript は正規表現リテラルの書き方を確かめる（まちがったものは tsc が
// 止める。check_literal.py を見る）が、グループの名前は型に入らない。
// groups は { [key: string]: string } なので、打ちまちがいは、動くときに
// undefined になる。regex-polish.md の [typed|…|] とくらべる

// ANCHOR: typos
// TypeScript
const date = /(?<year>\d{4})-(?<month>\d{2})/;
const m = "since 2026-11".match(date);
console.log(m?.groups?.year);
// a typo in the name: tsc says nothing
// 名前の打ちまちがい：tsc は何も言わない
console.log(m?.groups?.yaer);
// ANCHOR_END: typos

if (m?.groups?.year !== "2026" || m?.groups?.yaer !== undefined) {
  throw new Error("groups");
}
