// The TypeScript code in the chapter on the VM of MiniRegex: a search
// with no limit on its steps.
// MiniRegex の命令の列の章の TypeScript のコード：手数に上限のない探しもの
const names = ["Mira", "Ruby", "Lam", "GHC", "Perl"];
// (run it once first: the first run also compiles the pattern)
// （最初に1回動かしておく。最初の1回は、パターンのコンパイルもする）
/^(\w+\s?)+$/.test("Mira Ruby!");

// ANCHOR: nolimit
// TypeScript: no limit on the steps either; one more name, four times
//   as long
// TypeScript：こちらも手数に上限はない。名前が1つ増えると、4倍
const times: number[] = [];
for (const n of [7, 8, 9]) {
  const field = [...names, ...names].slice(0, n).join(" ") + "!";
  const start = performance.now();
  /^(\w+\s?)+$/.test(field);
  times.push(performance.now() - start);
  console.log(n, `${Math.round(times.at(-1)!)}ms`);
}
// ANCHOR_END: nolimit

if (!(times[2] > 2 * times[1])) {
  throw new Error(`not growing: ${times}`);
}
