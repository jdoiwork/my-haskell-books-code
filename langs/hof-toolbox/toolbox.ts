// The TypeScript code in the hof-toolbox chapter.
// 高階関数の道具箱の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
const attacks: number[] = [50, 35, 60];
const defenses: number[] = [20, 40, 10];
const hpLog: number[] = [100, 70, 40, 10, 0, 0];
const damage = (attack: number, defense: number): number =>
  Math.max(1, attack - defense);
{
  // ANCHOR: functional
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const results = attacks.map((a, i) => damage(a, defenses[i]));
  // ANCHOR_END: functional
  check(results.join() === "30,1,50", "zipWith");
}
{
  // ANCHOR: functional_2
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const i = hpLog.findIndex((hp) => hp <= 0);
  const cut = i === -1 ? hpLog.length : i;
  const [alive, after] = [hpLog.slice(0, cut), hpLog.slice(cut)];
  // ANCHOR_END: functional_2
  check(alive.join() === "100,70,40,10" && after.join() === "0,0", "span");
}
