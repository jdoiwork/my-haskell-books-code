// log2 in TypeScript, for the chapter "Exponents and logarithms".
// 「指数と対数」の章の log2 を、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: functional
// TypeScript
const exact = Math.log2(1000);          // 9.965784284662087
const halvings = 31 - Math.clz32(1000); // 9: count the leading zero bits
const doubling = 2 ** 30;               // 1073741824
// ANCHOR_END: functional

check(exact === 9.965784284662087, "log2");
check(halvings === 9, "clz32");
check(doubling === 1073741824, "2 ** 30");
console.log("ok");
