// The TypeScript code in the chapter on f"...": a template literal,
// with padStart, padEnd and toLocaleString for the format.
// f"..." の章の TypeScript のコード：テンプレートリテラル。
// 書式は padStart・padEnd・toLocaleString で

const prices: Record<string, number> = { sticker: 300, mug: 1200, tshirt: 2500 };
const orders: [string, number][] = [["sticker", 4], ["mug", 1], ["tshirt", 3]];

// ANCHOR: template
// TypeScript (template literal): ${...} puts a value in, but there is
// no format; call the methods of strings and numbers instead
// TypeScript（テンプレートリテラル）：${...} で値を入れる。書式はないので、
// 文字列や数のメソッドを呼ぶ
const yen = (n: number): string => n.toLocaleString("en-US");

function line(item: string, n: number): string {
  const price = prices[item] ?? 0;
  return `${item.padEnd(10)}${String(n).padStart(3)} x ` +
    `${yen(price).padStart(6)} = ${yen(n * price).padStart(9)}`;
}
// ANCHOR_END: template

const lines = orders.map(([item, n]) => line(item, n));
const expected = [
  "sticker     4 x    300 =     1,200",
  "mug         1 x  1,200 =     1,200",
  "tshirt      3 x  2,500 =     7,500",
];
if (JSON.stringify(lines) !== JSON.stringify(expected)) {
  throw new Error(`unexpected: ${JSON.stringify(lines)}`);
}
console.log(lines.join("\n"));
