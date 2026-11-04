// The TypeScript code in the expressions chapter.
// 式・型・関数の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const status = (hp: number) => (hp <= 0 ? "Defeated!" : "Still standing");
// ANCHOR_END: functional
check(status(0) === "Defeated!" && status(5) === "Still standing", "status");
const damage = (attack: number, defense: number): number =>
  Math.max(1, attack - defense);
// ANCHOR: functional_2
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const heroAttack = (defense: number) => damage(50, defense);
const results = [10, 30, 60].map(heroAttack);
// ANCHOR_END: functional_2
check(results.join() === "40,20,1", "heroAttack");
