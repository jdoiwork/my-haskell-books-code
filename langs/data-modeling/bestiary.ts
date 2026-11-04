// The TypeScript code in the data-modeling chapter.
// データをモデリングする章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
// ANCHOR: union_type
// TypeScript (union type)
// TypeScript（ユニオン型）
type Element = "fire" | "water" | "grass";
// ANCHOR_END: union_type
// ANCHOR: discriminated_union
// TypeScript (discriminated union)
// TypeScript（判別共用体）
type Action =
  | { kind: "attack"; power: number }
  | { kind: "spell"; element: Element }
  | { kind: "heal"; amount: number }
  | { kind: "defend" };

function describe(action: Action): string {
  switch (action.kind) {
    case "attack":
      return `Attack with power ${action.power}`;
    case "spell":
      return `Cast a ${action.element} spell`;
    case "heal":
      return `Heal ${action.amount} HP`;
    case "defend":
      return "Defend";
  }
}
// ANCHOR_END: discriminated_union
check(describe({ kind: "attack", power: 30 }) === "Attack with power 30", "attack");
check(describe({ kind: "spell", element: "fire" }) === "Cast a fire spell", "spell");
check(describe({ kind: "defend" }) === "Defend", "defend");
