// The TypeScript code in the chapter on strings, tuples and lists.
// 文字列・タプル・リストの章の TypeScript のコード

// ANCHOR: thunk
// TypeScript (functional style): the right side, wrapped in () =>
// TypeScript（関数型スタイル）：右側を () => で包む
const lazyAnd = (a: boolean, b: () => boolean): boolean =>
  a ? b() : false;

const boom = (): boolean => {
  throw new Error("computed!");
};
console.log(false && boom());        // false: && does not call boom
console.log(lazyAnd(false, boom));   // false: lazyAnd does not call it
// ANCHOR_END: thunk

if (lazyAnd(false, boom) !== false) throw new Error("lazyAnd(false)");
if (lazyAnd(true, () => true) !== true) throw new Error("lazyAnd(true)");
