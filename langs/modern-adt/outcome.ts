// The outcome of a quest, for the chapter "ADTs are everywhere".
// 依頼の結果。「代数的データ型は、どこにでもある」の章
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: union
// TypeScript (discriminated union and never)
// TypeScript（判別できる union と never）
type Outcome =
  | { kind: "cleared"; gold: number }
  | { kind: "failed"; place: string }
  | { kind: "fled" };

function describe(outcome: Outcome): string {
  switch (outcome.kind) {
    case "cleared":
      return `cleared! +${outcome.gold} G`;
    case "failed":
      return `failed at ${outcome.place}`;
    case "fled":
      return "ran away";
    default: {
      // Only reachable if a case is missing; then this line fails
      // to type-check.
      // 場合が足りないときだけ届く。そのときは、この行の型検査が通らない
      const missing: never = outcome;
      return missing;
    }
  }
}
// ANCHOR_END: union

check(describe({ kind: "cleared", gold: 50 }) === "cleared! +50 G", "cleared");
check(describe({ kind: "failed", place: "cave" }) === "failed at cave", "failed");
check(describe({ kind: "fled" }) === "ran away", "fled");
console.log("ok");
