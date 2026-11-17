// ANCHOR: all
// JavaScript: a name that is not exported cannot be imported.
// Node stops before running anything (a SyntaxError).
// JavaScript：export していない名前は import できない。
// Node は、何も動かす前に止まる（SyntaxError）
import { make } from "./price.mjs";

console.log(make(-5));
// ANCHOR_END: all
