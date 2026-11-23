// 🧾 Where is the + on this line? JavaScript's strings are UTF-16, so
//    .length already counts as LSP does.
// この行の + は、どこ？ JavaScript の文字列は UTF-16 なので、.length が
// もう LSP と同じ数え方
const line = 'f"🧾 total: {total items + 100}"';
const before = line.slice(0, line.indexOf("+"));

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function letterCount(text: string): number {
  let count = 0;
  for (const _ of text) {
    count += 1;
  }
  return count;
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const letters = [...before].length;                       // 24
const utf16 = before.length;                              // 25
const utf8 = new TextEncoder().encode(before).length;     // 27
// ANCHOR_END: functional

if (letterCount(before) !== letters) throw new Error("letters");
if (letters !== 24 || utf16 !== 25 || utf8 !== 27) {
  throw new Error(`${letters} ${utf16} ${utf8}`);
}
console.log("columns.ts: ok");
