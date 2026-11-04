// The TypeScript code in the Map chapter.
// Map の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
const bag: Record<string, number> = { potion: 3, herb: 1, elixir: 0 };
// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const newBag = { ...bag, potion: (bag.potion ?? 0) + 2 };
// ANCHOR_END: functional
check(newBag.potion === 5 && bag.potion === 3, "newBag");
