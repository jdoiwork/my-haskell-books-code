// The TypeScript code in the branching chapter.
// 場合分けと Maybe の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function category(item: string): string {
  switch (item) {
    case "potion":
    case "elixir":
      return "medicine";
    case "sword":
      return "weapon";
    case "shield":
      return "armor";
    default:
      return "unknown";
  }
}
// ANCHOR_END: procedural
check(["elixir", "sword", "shield", "bow"].map(category).join() === "medicine,weapon,armor,unknown", "category");
// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const prices = new Map([["potion", 50], ["elixir", 150], ["sword", 300]]);

const priceTag = (item: string): string => {
  const p: number | undefined = prices.get(item);
  return p === undefined ? `${item}: not for sale` : `${item}: ${p} G`;
};
// ANCHOR_END: functional
check(priceTag("sword") === "sword: 300 G" && priceTag("bow") === "bow: not for sale", "priceTag");
