// The TypeScript code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: typed
// TypeScript (a typed array: one byte per number, rewritten in place)
// TypeScript（型付き配列：1つの数に1バイト。その場で書き換える）
function sieve(n: number): Uint8Array {
  const isPrime = new Uint8Array(n + 1).fill(1);
  isPrime[0] = 0;
  isPrime[1] = 0;
  for (let p = 2; p * p <= n; p++) {
    if (isPrime[p] === 1) {
      for (let m = p * p; m <= n; m += p) {
        isPrime[m] = 0;
      }
    }
  }
  return isPrime;
}
// ANCHOR_END: typed

const primes: number[] = [];
sieve(30).forEach((ok, i) => { if (ok === 1) primes.push(i); });
check(primes.join() === "2,3,5,7,11,13,17,19,23,29", "primes");
check(sieve(1000000).reduce((a, b) => a + b, 0) === 78498, "count");
console.log("ok");
