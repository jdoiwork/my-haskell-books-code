// The TypeScript code in the laziness chapter.
// 遅延評価の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: lazy
// TypeScript (a generator function: function*)
// TypeScript（ジェネレーター関数：function*）
function* doubled(): Generator<number> {
  for (let n = 1; ; n++) {
    console.log(`making ${n * 2}`);
    yield n * 2;
  }
}

const first3: number[] = [];
for (const d of doubled()) {
  first3.push(d);
  if (first3.length === 3) break;
}
// ANCHOR_END: lazy
check(JSON.stringify(first3) === "[2,4,6]", "first3");

// ANCHOR: strings
// TypeScript (length counts UTF-16 code units)
// TypeScript（length は UTF-16 の単位の数）
check("冒険".length === 2, "kanji");
check("😀".length === 2, "emoji");
check([..."😀"].length === 1, "code points");
// ANCHOR_END: strings
