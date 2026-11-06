// The guild's key rule of the chapter "Logic and sets", in TypeScript.
// 「論理と集合」の章のギルドの鍵の決まりを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: functional
// TypeScript
const keyRuleOk = (level: number, hasKey: boolean): boolean =>
  !hasKey || level >= 10;
// ANCHOR_END: functional

check(keyRuleOk(15, false), "a veteran without a key");
check(keyRuleOk(12, true), "a veteran with a key");
check(!keyRuleOk(5, true), "a beginner with a key");
check(keyRuleOk(5, false), "a beginner without a key");
console.log("ok");
