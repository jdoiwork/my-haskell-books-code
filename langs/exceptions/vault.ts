// The TypeScript code in the "IO exceptions" chapter.
// IO の例外をちゃんと扱うの章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const log: string[] = [];

class GoblinAlarm extends Error {}

function openDoor(): void {
  log.push("door: open");
}

function closeDoor(): void {
  log.push("door: closed");
}

function countCoins(): void {
  log.push("counting: 1, 2, 3, ...");
  throw new GoblinAlarm();
}

// ANCHOR: finally
// TypeScript (procedural style: try / finally)
// TypeScript（手続き型スタイル：try / finally）
function shift(work: () => void): void {
  openDoor();
  try {
    work();
  } finally {
    closeDoor();
  }
}
// ANCHOR_END: finally

try {
  shift(countCoins);
} catch (e) {
  check(e instanceof GoblinAlarm, "the alarm goes up");
}
check(log.join("|") === "door: open|counting: 1, 2, 3, ...|door: closed",
      log.join("|"));
log.length = 0;

// ANCHOR: using
// TypeScript (the using declaration: closed at the end of the block)
// TypeScript（using 宣言：ブロックの終わりで閉じる）
function inVault(): Disposable {
  openDoor();
  return { [Symbol.dispose]: closeDoor };
}

function nightShift(): void {
  using door = inVault();
  countCoins();
}
// ANCHOR_END: using

try {
  nightShift();
} catch (e) {
  check(e instanceof GoblinAlarm, "the alarm goes up");
}
check(log.join("|") === "door: open|counting: 1, 2, 3, ...|door: closed",
      log.join("|"));
console.log("ok");
