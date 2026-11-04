// The TypeScript code in the higher-order chapter.
// 高階関数の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
const enemies: number[] = [30, 5, 80, 12, 45];
const hps: number[] = [5, 0, 55, 0, 20];
{
  // ANCHOR: functional
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const total = hps.reduce((acc, hp) => acc + hp, 0);
  // ANCHOR_END: functional
  check(total === 80, "line 319");
}
{
  // ANCHOR: functional_2
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const afterAttack = enemies.map((hp) => Math.max(0, hp - 25));
  const alive = afterAttack.filter((hp) => hp > 0);
  const total = alive.reduce((acc, hp) => acc + hp, 0);
  // ANCHOR_END: functional_2
  check(total === 80, "line 546");
}
{
  // ANCHOR: functional_3
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const total = enemies
    .map((hp) => Math.max(0, hp - 25))
    .filter((hp) => hp > 0)
    .reduce((acc, hp) => acc + hp, 0);
  // ANCHOR_END: functional_3
  check(total === 80, "line 639");
}
{
  // ANCHOR: functional_4
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const stats = hps.reduce(
    (acc, hp) =>
      hp > 0
        ? { count: acc.count + 1, total: acc.total + hp }
        : acc,
    { count: 0, total: 0 },
  );
  // ANCHOR_END: functional_4
  check(stats.count === 3 && stats.total === 80, "stats");
}
