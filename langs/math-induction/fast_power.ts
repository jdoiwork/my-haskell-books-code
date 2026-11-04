// Fast power with its loop invariant checked, in TypeScript (bigint).
// 速いべき乗を、ループ不変条件を確かめながら、TypeScript で（bigint）
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function power(x: bigint, n: bigint): bigint {
  let acc = 1n, b = x, e = n;
  while (e > 0n) {
    check(acc * b ** e === x ** n, "invariant");
    if (e % 2n === 0n) {
      b = b * b;
      e = e / 2n;
    } else {
      acc = acc * b;
      e = e - 1n;
    }
  }
  return acc;
}
// ANCHOR_END: procedural

check(power(2n, 10n) === 1024n, "2^10");
check(power(3n, 5n) === 243n, "3^5");
console.log("ok");
