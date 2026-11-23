// The TypeScript code in the chapter "Turning Lam into JavaScript".
// 「JavaScript に変換する」の章の TypeScript のコード

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function factorial(n: number): number {
  let total = 1;
  for (let i = 1; i <= n; i++) {
    total *= i;
  }
  return total;              // a number: past 2^53, digits go
}

// BigInt: write n at the end, and the digits stay.
// BigInt：後ろに n を書けば、桁は消えない
function factorialBig(n: bigint): bigint {
  let total = 1n;
  for (let i = 1n; i <= n; i++) {
    total *= i;
  }
  return total;
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const factorialBig2 = (n: number): bigint =>
  Array.from({ length: n }, (_, i) => BigInt(i + 1))
    .reduce((a, b) => a * b, 1n);

// BigInt's / goes toward 0. Rounding down is up to you.
// BigInt の / は 0 の方へ。下へ丸めるのは自分で
const floorDiv = (a: bigint, b: bigint): bigint =>
  a % b !== 0n && (a < 0n) !== (b < 0n) ? a / b - 1n : a / b;
// ANCHOR_END: functional

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(what);
}
check(factorial(25) === 1.5511210043330986e25, "number");
check(String(factorial(25)) === "1.5511210043330986e+25", "text");
check(factorialBig(25n) === 15511210043330985984000000n, "bigint");
check(factorialBig2(25) === factorialBig(25n), "reduce");
check(-7n / 2n === -3n && floorDiv(-7n, 2n) === -4n, "division");
check(floorDiv(7n, 2n) === 3n && floorDiv(7n, -2n) === -4n, "signs");
console.log(factorial(25), factorialBig(25n), floorDiv(-7n, 2n));
