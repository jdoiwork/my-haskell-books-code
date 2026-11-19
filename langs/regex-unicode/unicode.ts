// The TypeScript code in the Unicode chapter of MiniRegex: \d stays
// ASCII, \p{...} with the u flag, case without and with u, (?i:...)
// and places counted in UTF-16.
// MiniRegex の Unicode の章の TypeScript のコード：\d は ASCII のまま、
// u フラグの \p{...}、u のあるなしでちがう大文字と小文字、(?i:...)、
// UTF-16 で数える場所
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

const record = "２０２６年１１月１９日、2026-11-19";

// ANCHOR: digits
// TypeScript: \d is 0 to 9 only, even with u; \p{Nd} is every digit
// TypeScript：\d は、u を付けても 0 から 9 だけ。\p{Nd} は全部の数字
console.log(record.match(/\d{4}/gu));
console.log(record.match(/\p{Nd}{4}/gu));
// ANCHOR_END: digits

same(record.match(/\d{4}/gu), ["2026"]);
same(record.match(/\p{Nd}{4}/gu), ["２０２６", "2026"]);

// ANCHOR: scripts
// TypeScript: Script is the script of the character; Script_Extensions
//   takes ー (used with kana) too
// TypeScript：Script は文字の文字体系。Script_Extensions なら、
//   かなと一緒に使う ー も入る
console.log("ノート".match(/\p{Script=Katakana}+/u)?.[0]);
console.log("ノート".match(/\p{Script_Extensions=Katakana}+/u)?.[0]);
// ANCHOR_END: scripts

same("ノート".match(/\p{Script=Katakana}+/u)?.[0], "ノ");
same("ノート".match(/\p{Script_Extensions=Katakana}+/u)?.[0], "ノート");

// ANCHOR: caseless
// TypeScript: without u, i knows K (Kelvin) is not k; with u, it is
// TypeScript：u がなければ、i は K（ケルビン）を k と思わない。u なら思う
console.log(/k/i.test("K"), /k/iu.test("K"));
console.log(/ß/iu.test("ẞ"), /ß/iu.test("SS"), /i/iu.test("İ"));
// ANCHOR_END: caseless

same([/k/i.test("K"), /k/iu.test("K")], [false, true]);
same([/ß/iu.test("ẞ"), /ß/iu.test("SS"), /i/iu.test("İ")],
  [true, false, false]);

// ANCHOR: flags
// TypeScript: (?i:...) for a part (ES2025); (?i) alone is not there
// TypeScript：一部分に (?i:...)（ES2025 から）。(?i) だけの形はない
console.log(/(?i:a)b/.test("Ab"), /(?i:a)b/.test("AB"));
// ANCHOR_END: flags

same([/(?i:a)b/.test("Ab"), /(?i:a)b/.test("AB")], [true, false]);

// ANCHOR: places
// TypeScript: a string counts UTF-16 units; 🐉 is two of them
// TypeScript：文字列は UTF-16 の単位で数える。🐉 は2つぶん
const dragon = "🐉 Lam";
console.log(dragon.search(/Lam/u), [...dragon.slice(0, 3)].length);
// ANCHOR_END: places

same([dragon.search(/Lam/u), [...dragon.slice(0, 3)].length], [3, 2]);
