// The party's skills of the chapter "Logic and sets", in TypeScript.
// 「論理と集合」の章のパーティーのスキルを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
const needed = new Set(["heal", "fire", "stealth"]);
const members = [
  new Set(["fire", "sword"]),
  new Set(["heal", "fire"]),
  new Set(["fire", "shield"]),
];

// ANCHOR: functional
// TypeScript (functional style, Set methods from ES2025)
// TypeScript（関数型スタイル。ES2025 の Set のメソッド）
const party = members.reduce((a, b) => a.union(b));
const missing = needed.difference(party);
const shared = members.reduce((a, b) => a.intersection(b));
const everyoneFights = members.every((m) => m.has("fire"));
// ANCHOR_END: functional

check([...missing].join() === "stealth", "missing");
check([...shared].join() === "fire", "shared");
check(everyoneFights, "every");
check([].every((x) => x > 0) && ![].some((x) => x > 0), "empty");
console.log("ok");
