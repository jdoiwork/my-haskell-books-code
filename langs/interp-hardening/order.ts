// TypeScript: a function declaration is hoisted, so it can be called
// above it. A const arrow function is not.
// TypeScript：関数宣言は巻き上げられるので、上から呼べる。const の
// アロー関数は、そうはいかない

// ANCHOR: order
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
console.log(isEven(10), isOdd(7));

function isEven(n: number): boolean {
  if (n === 0) {
    return true;
  }
  return isOdd(n - 1);
}

function isOdd(n: number): boolean {
  if (n === 0) {
    return false;
  }
  return isEven(n - 1);
}
// ANCHOR_END: order

// ANCHOR: tdz
// A const used before its line runs: a ReferenceError at run time.
// 行が動く前に const を使うと、実行のときに ReferenceError
function tooEarly(): string {
  return suffix;
}
let caught = "";
try {
  tooEarly();
} catch (e) {
  caught = (e as Error).name;
}
const suffix = "!";
// ANCHOR_END: tdz

if (caught !== "ReferenceError") {
  throw new Error("expected a ReferenceError, got " + caught);
}
if (!(isEven(10) && isOdd(7)) || tooEarly() !== "!") {
  throw new Error("wrong");
}
