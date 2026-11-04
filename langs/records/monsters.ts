// The TypeScript code in the records chapter.
// レコードと newtype の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
type Element = "fire" | "water" | "grass";
// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
type Monster = { name: string; hp: number; element: Element };

const hitMonster = (atk: number, monster: Monster): Monster => ({
  ...monster,
  hp: Math.max(0, monster.hp - atk),
});
// ANCHOR_END: functional
const slime: Monster = { name: "Slime", hp: 30, element: "water" };
const hit = hitMonster(10, slime);
check(hit.hp === 20 && slime.hp === 30 && hit.name === "Slime", "hitMonster");
check(hitMonster(100, slime).hp === 0, "not below 0");
