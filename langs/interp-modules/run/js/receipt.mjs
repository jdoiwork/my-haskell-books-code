// ANCHOR: all
// JavaScript: receipt.mjs, next to price.mjs
// JavaScript：receipt.mjs（price.mjs の隣）
import * as P from "./price.mjs";      // P.fromYen   (like  as P)
import { show } from "./price.mjs";    // show        (like  exposing)

const prices = [P.fromYen(1200), P.fromYen(350), P.fromYen(-5)];
for (const m of prices) {
  console.log(m === null ? "not a price" : show(m));
}
console.log(P.make);   // undefined: it is not exported
// ANCHOR_END: all
