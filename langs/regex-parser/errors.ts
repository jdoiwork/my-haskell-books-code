// The TypeScript code in the parser chapter of MiniRegex: how RegExp
// tells a wrong pattern, and a { that is not a quantifier.
// MiniRegex のパーサーの章の TypeScript のコード：RegExp が、まちがった
// パターンをどう知らせるか。量指定子でない {
// Throw when the two differ.
// 2つがちがったら投げる
function same(actual: unknown, expected: unknown): void {
  if (actual !== expected) {
    throw new Error(`unexpected: ${String(actual)}`);
  }
}

// ANCHOR: errors
// TypeScript: new RegExp throws a SyntaxError, with a message only
// TypeScript：new RegExp は SyntaxError を投げる。文だけで、場所はない
function tell(pattern: string, flags = ""): string {
  try {
    new RegExp(pattern, flags);
    return "ok";
  } catch (e) {
    return e instanceof SyntaxError ? e.message : "?";
  }
}

for (const p of ["a{5,2}", "[z-a]", "(ab", "ab)", "a**"]) {
  console.log(tell(p));
}
// ANCHOR_END: errors

same(
  tell("a{5,2}"),
  "Invalid regular expression: /a{5,2}/: " +
    "numbers out of order in {} quantifier",
);
same(tell("a**"), "Invalid regular expression: /a**/: Nothing to repeat");

// ANCHOR: braces
// TypeScript: without the u flag, x{a} and a{,3} are the characters;
// with it, they are errors
// TypeScript：u のフラグがなければ、x{a} も a{,3} もただの文字。
// u を付けると、エラー
console.log(/x{a}/.test("x{a}"), /a{,3}/.test("a{,3}"));
console.log(tell("x{a}", "u"));
console.log(tell("a{,3}", "u"));
// ANCHOR_END: braces

same(/x{a}/.test("x{a}"), true);
same(/^a{,3}$/.test("aaa"), false);
same(tell("x{a}", "u"),
  "Invalid regular expression: /x{a}/u: Incomplete quantifier");
