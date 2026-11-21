// The TypeScript code in the emoji chapter of MiniRegex: length counts
// UTF-16 units, [...s] counts code points, Intl.Segmenter counts what a
// person sees, and the v flag's \p{RGI_Emoji} takes a whole emoji.
// MiniRegex の絵文字の章の TypeScript のコード：length は UTF-16 の単位を、
// [...s] はコードポイントを、Intl.Segmenter は人が見る文字を数える。
// v フラグの \p{RGI_Emoji} は、絵文字を丸ごと取る
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

// 👨‍👩‍👧: a man, a woman and a girl, with ZWJs between
// 👨‍👩‍👧：男の人、女の人、女の子のあいだに ZWJ
const family = "\u{1F468}‍\u{1F469}‍\u{1F467}";

// ANCHOR: count
// TypeScript: three ways to count the same family
// TypeScript：同じ家族の、3つの数え方
const segmenter = new Intl.Segmenter("ja", { granularity: "grapheme" });
console.log(family.length);
console.log([...family].length);
console.log([...segmenter.segment(family)].length);
// ANCHOR_END: count

same(family.length, 8);
same([...family].length, 5);
same([...segmenter.segment(family)].length, 1);

// ANCHOR: emoji
// TypeScript: \p{Emoji} takes 7 and #, too; with the v flag,
//   \p{RGI_Emoji} takes the whole family at once
// TypeScript：\p{Emoji} は 7 や # も取る。v フラグなら、
//   \p{RGI_Emoji} が家族を丸ごと1つで取る
const post = `No.7 #1 ${family}`;
console.log(post.match(/\p{Emoji}/gu));
console.log(post.match(/\p{RGI_Emoji}/gv)?.length);
// ANCHOR_END: emoji

same(post.match(/\p{Emoji}/gu), ["7", "#", "1", "👨", "👩", "👧"]);
same(post.match(/\p{RGI_Emoji}/gv), [family]);
