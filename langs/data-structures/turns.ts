// The TypeScript code in the data structures chapter.
// データ構造の選び方の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: array
// TypeScript (an array: shift() moves every other item)
// TypeScript（配列：shift() は、残りを全部ずらす）
function takeTurns(n: number, heroes: string[]): string[] {
  const line = [...heroes];
  const order: string[] = [];
  for (let i = 0; i < n; i++) {
    const hero = line.shift();
    if (hero === undefined) break;
    order.push(hero);
    line.push(hero);
  }
  return order;
}
// ANCHOR_END: array

check(takeTurns(7, ["Lambda", "Python", "Rust"]).join() ===
      "Lambda,Python,Rust,Lambda,Python,Rust,Lambda", "turns");
