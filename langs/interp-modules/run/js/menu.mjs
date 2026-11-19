// ANCHOR: all
// JavaScript: menu.mjs, next to price.mjs
// JavaScript：menu.mjs（price.mjs の隣）
// The two lines together are Lam's  import Price as P (fromYen).
// 2行で、Lam の  import Price as P (fromYen)  と同じ
import * as P from "./price.mjs";       // P.fromYen, P.add
import { fromYen } from "./price.mjs";  // fromYen

const prices = [fromYen(1200), fromYen(350), P.fromYen(-5)];
for (const m of prices) {
  console.log(m === null ? "not a price" : `${m}`);
}
console.log(P.make);   // undefined: it is not exported
// ANCHOR_END: all
