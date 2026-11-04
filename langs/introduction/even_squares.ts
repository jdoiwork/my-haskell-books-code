// The comparison code in the introduction.
// 「はじめに」の比較のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
{
  // ANCHOR: procedural
  // TypeScript (procedural style)
  // TypeScript（手続き型スタイル）
  const evenSquares: number[] = [];
  for (let x = 1; x <= 10; x++) {
    if (x % 2 === 0) {
      evenSquares.push(x * x);
    }
  }
  // ANCHOR_END: procedural
  check(evenSquares.join() === "4,16,36,64,100", "procedural");
}
{
  // ANCHOR: functional
  // TypeScript (functional style)
  // TypeScript（関数型スタイル）
  const evenSquares = Array.from({ length: 10 }, (_, i) => i + 1)
    .filter((x) => x % 2 === 0)
    .map((x) => x * x);
  // ANCHOR_END: functional
  check(evenSquares.join() === "4,16,36,64,100", "functional");
}
