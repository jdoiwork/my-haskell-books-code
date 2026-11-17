// ANCHOR: all
// JavaScript (an ES module): price.mjs
// JavaScript（ES モジュール）：price.mjs
// make is not exported: no other file can call it.
// make は export しない。ほかのファイルからは呼べない
const make = (yen) => Object.freeze({ yen });

export function fromYen(n) {
  return n < 0 ? null : make(n);
}

export function add(a, b) {
  return make(a.yen + b.yen);
}

export function show(p) {
  return `${p.yen.toLocaleString("en-US")} yen`;
}
// ANCHOR_END: all
