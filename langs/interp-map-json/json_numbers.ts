// The TypeScript code in the chapter on Map and JSON: the numbers of JSON.
// Map と JSON の章の TypeScript のコード：JSON の数

// ANCHOR: parse
// TypeScript: JSON.parse makes every number a number (a 64-bit float)
// TypeScript：JSON.parse は、どの数も number（64ビットの浮動小数点数）にする
const prices: number[] = JSON.parse("[0.1, 0.2]");
const total = prices.reduce((sum, p) => sum + p, 0);
console.log(total); // 0.30000000000000004

const id: number = JSON.parse("12345678901234567890");
console.log(id); // 12345678901234567000
// ANCHOR_END: parse

if (total !== 0.30000000000000004) throw new Error("total");
if (String(id) === "12345678901234567890") throw new Error("id");
