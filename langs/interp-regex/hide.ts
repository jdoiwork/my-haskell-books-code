// The TypeScript code in the chapter on regular expressions:
// replace with a function, and a named group.
// 正規表現の章の TypeScript のコード：関数で置き換える。名前付きのグループ
const text = "call 080-9876-5432 or 03-1234-5678 now";
const want = "call ***-****-5432 or ***-****-5678 now";

// ANCHOR: replace
// TypeScript: /.../g finds every match; the last argument of the
// function holds the named groups
// TypeScript：/.../g で全部を探す。関数の最後の引数に、名前付きのグループ
const phone = /\d{2,4}-\d{2,4}-(?<last>\d{4})/g;

function hide(text: string): string {
  return text.replace(phone, (...args) => {
    const groups: { last: string } = args[args.length - 1];
    return `***-****-${groups.last}`;
  });
}
// ANCHOR_END: replace

// Check an answer. / 答えを確かめる
function same(actual: string, expected: string): void {
  if (actual !== expected) {
    throw new Error(`unexpected: ${actual}`);
  }
}

same(hide(text), want);
// $<last> in the replacement string means the group.
// 置き換えの文字列の $<last> は、グループのこと
same(text.replace(phone, "***-****-$<last>"), want);
// Without g, only the first one.
// g がなければ、最初の1つだけ
same(
  text.replace(/\d{2,4}-\d{2,4}-\d{4}/, "#"),
  "call # or 03-1234-5678 now",
);
console.log(hide(text));
