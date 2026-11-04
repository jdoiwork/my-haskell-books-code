// The numbers of the chapter "What numbers really are", in TypeScript.
// 「数の正体」の章の数を、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: numbers
// TypeScript
const safe = Number.MAX_SAFE_INTEGER;   // 2 ** 53 - 1
const lost = 2 ** 53 + 1;               // 9007199254740992 (the +1 is lost)
const truncDiv = Math.trunc(-7 / 2);    // -3  (like quot)
const truncRem = -7 % 2;                // -1  (like rem)
const half = Math.round(2.5);           // 3   (up / 上へ)
const big = 2n ** 100n;                 // BigInt never overflows
// ANCHOR_END: numbers

check(safe === 9007199254740991, "safe");
check(lost === 9007199254740992, "lost");
check(truncDiv === -3 && truncRem === -1, "trunc");
check(half === 3 && Math.round(-2.5) === -2, "round");
check(big === 1267650600228229401496703205376n, "big");
check(0.1 + 0.2 !== 0.3, "0.1 + 0.2");
console.log("ok");
