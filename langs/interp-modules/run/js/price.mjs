// ANCHOR: all
// JavaScript (an ES module): price.mjs
// JavaScript（ES モジュール）：price.mjs
// make is not exported: no other file can call it. toString is Lam's
// instance Display Price: `${p}` calls it.
// make は export しない。ほかのファイルからは呼べない。toString は、
// Lam の instance Display Price と同じ。`${p}` が呼ぶ
const make = (yen) =>
  Object.freeze({
    yen,
    toString: () => `${yen.toLocaleString("en-US")} yen`,
  });

export function fromYen(n) {
  return n < 0 ? null : make(n);
}

export function add(a, b) {
  return make(a.yen + b.yen);
}
// ANCHOR_END: all
