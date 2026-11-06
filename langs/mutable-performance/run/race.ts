// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   node race.ts 10000000

// Only the part of Node's process we use (no @types/node).
// Node の process のうち、使う分だけの型（@types/node は入れない）
declare const process: { argv: string[] };
function sieve(n: number): number {
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
  let count = 0;
  for (let i = 0; i <= n; i++) count += isPrime[i];
  return count;
}

const n = Number(process.argv[2]);
let best = Infinity;
let count = 0;
for (let i = 0; i < 3; i++) {
  const start = performance.now();
  count = sieve(n);
  best = Math.min(best, performance.now() - start);
}
console.log(count, best.toFixed(1));
