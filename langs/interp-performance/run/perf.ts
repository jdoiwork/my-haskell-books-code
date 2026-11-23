// TypeScript (Node): the programs of examples/perf of the chapter,
// written by hand as TypeScript is usually written (not turned from Lam).
//   node perf.ts loop | fib | range | sort | deep | count FILE | slow FILE
// TypeScript（Node）：章の examples/perf のプログラムを、TypeScript で
// ふつうに書いたもの（Lam から変換したものではない）
// Only the part of Node's process we use (no @types/node).
// Node の process のうち、使う分だけの型（@types/node は入れない）
type Fs = { readFileSync(path: string, encoding: "utf8"): string };
declare const process: {
  argv: string[];
  exitCode: number | undefined;
  stderr: { write(text: string): void };
  getBuiltinModule(name: "node:fs"): Fs;
};
const fs = process.getBuiltinModule("node:fs");

// ANCHOR: loop
// TypeScript
// A million steps, with a for loop.
// 百万歩。for のループで
function loop(): number {
  let total = 0;
  for (let n = 1; n <= 1_000_000; n++) total += n;
  return total;
}
// ANCHOR_END: loop

function fib(n: number): number {
  return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

// [1, 2, 3] as Lam and Python write a list.
// Lam と Python と同じ書き方のリスト
const shown = (xs: number[]) => `[${xs.join(", ")}]`;

// ANCHOR: deep
// TypeScript
function deep(): void {
  console.log(loop());
  const xs = Array.from({ length: 200_000 }, (_, i) => i + 1);
  console.log(xs.map((x) => x * 2).reduce((a, b) => a + b, 0));
  console.log([...xs].reverse().sort((a, b) => a - b).length);
}
// ANCHOR_END: deep

// The lines of a file: Node reads it at once and cuts it.
// ファイルの行。Node では一度に読んで、切る
function linesOf(path: string): string[] {
  const text = fs.readFileSync(path, "utf8");
  const lines = text.split("\n");
  if (lines.at(-1) === "") lines.pop();
  return lines;
}

// ANCHOR: slow
// TypeScript
const line =
  /^(?<ip>\S+) .*" (?<status>\d{3}) \d+ rt=(?<rt>[\d.]+)$/;

function slow(path: string): void {
  const counts = new Map<string, number>();
  for (const text of linesOf(path)) {
    const g = line.exec(text)?.groups;
    if (g && Number(g.status) >= 500 && Number(g.rt) > 0.5) {
      counts.set(g.ip, (counts.get(g.ip) ?? 0) + 1);
    }
  }
  // The biggest first; the same counts by IP, as Lam's Map gives them.
  // 大きい順。同じ数は、Lam の Map と同じく IP の順
  const top = [...counts]
    .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
    .sort(([, m], [, n]) => n - m)
    .slice(0, 5);
  for (const [ip, n] of top) console.log(`${String(n).padStart(5)}  ${ip}`);
}
// ANCHOR_END: slow

const [task, path] = process.argv.slice(2);
switch (task) {
  case "loop": console.log(loop()); break;
  case "fib": console.log(fib(27)); break;
  case "range": {
    let total = 0;
    for (let x = 1; x <= 1_000_000; x++) total += x;
    console.log(total);
    break;
  }
  case "sort": {
    const xs = Array.from(
      { length: 100_000 },
      (_, i) => (i + 1) * 7919 % 100003,
    );
    const ys = [...xs].sort((a, b) => a - b);
    console.log(shown(ys.slice(0, 5)));
    console.log(ys.length);
    break;
  }
  case "deep": deep(); break;
  case "count": console.log(linesOf(path).length); break;
  case "slow": slow(path); break;
  default:
    process.stderr.write(
      "usage: perf.ts loop|fib|range|sort|deep|count F|slow F\n",
    );
    process.exitCode = 2;
}
