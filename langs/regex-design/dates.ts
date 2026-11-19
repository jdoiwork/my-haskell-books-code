// The TypeScript code in the design chapter of MiniRegex: the RegExp of
// JavaScript, and how its answers look.
// MiniRegex の設計の章の TypeScript のコード：JavaScript の RegExp と、
// その答えの形
// Throw when the two differ (compared as JSON).
// 2つがちがったら投げる（JSON にしてくらべる）
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

const records = `2026-11-19 Mira sold a dragon scale: 300 gold
2026-11-20 Ruby bought herb, sword, shield
On 2026-11-21 a dragon was seen near the lake
Room 1234-56-789 is closed
`;

// ANCHOR: api
// TypeScript: test, matchAll, replaceAll, split
// TypeScript：test・matchAll・replaceAll・split
const date = /[0-9]{4}-[0-9]{2}-[0-9]{2}/g;
const all = [...records.matchAll(date)].map((m) => m[0]);
console.log(all);
console.log(/dragon/.test(records));
console.log(records.replaceAll(/dragon/g, "wyvern").split("\n")[0]);
console.log("herb, sword, shield".split(/, /));
// ANCHOR_END: api

same(all, [
  "2026-11-19", "2026-11-20", "2026-11-21", "1234-56-78",
]);
same("herb, sword, shield".split(/, /), ["herb", "sword", "shield"]);

// ANCHOR: shapes
// TypeScript: match gives one match with its place, or, with the g
// flag, just the strings
// TypeScript：match は場所つきの一致1つ。g のフラグを付けると文字列だけ
const one = "2026-11 2027-01".match(/[0-9]{4}-[0-9]{2}/);
const many = "2026-11 2027-01".match(/[0-9]{4}-[0-9]{2}/g);
console.log(one?.[0], one?.index);
console.log(many);
// ANCHOR_END: shapes

same(one?.[0], "2026-11");
same(one?.index, 0);
same(many, ["2026-11", "2027-01"]);

// ANCHOR: lastindex
// TypeScript: a g regex remembers where it stopped (lastIndex), and
// matchAll starts from there
// TypeScript：g の付いた正規表現は、止まった場所（lastIndex）を覚えて
// いて、matchAll は、そこから探し始める
const again = /[0-9]{4}-[0-9]{2}-[0-9]{2}/g;
console.log(again.test(records));
console.log([...records.matchAll(again)].map((m) => m[0]));
// ANCHOR_END: lastindex

{
  const d = /[0-9]{4}-[0-9]{2}-[0-9]{2}/g;
  same(d.test(records), true);
  same([...records.matchAll(d)].map((m) => m[0]),
       ["2026-11-20", "2026-11-21", "1234-56-78"]);
}

// ANCHOR: error
// TypeScript: a wrong pattern throws a SyntaxError when it is made
// TypeScript：まちがったパターンは、作ったときに SyntaxError を投げる
try {
  new RegExp("ab\\");
} catch (e) {
  console.log((e as Error).name);
}
// ANCHOR_END: error

let thrown = "";
try {
  new RegExp("ab\\");
} catch (e) {
  thrown = (e as Error).name;
}
same(thrown, "SyntaxError");
same("abc".split(/(?:)/), ["a", "b", "c"]);
