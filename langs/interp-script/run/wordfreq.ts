#!/usr/bin/env node
// ANCHOR: ts
// TypeScript (Node, functional style)
// TypeScript（Node、関数型スタイル）
// ANCHOR_END: ts
// Only the part of Node's process we use (no @types/node).
// Node の process のうち、使う分だけの型（@types/node は入れない）
type Fs = { readFileSync(path: string | number, encoding: "utf8"): string };
declare const process: {
  argv: string[];
  exitCode: number | undefined;
  stderr: { write(text: string): void };
  getBuiltinModule(name: "node:fs"): Fs;
};
// ANCHOR: tsBody
const fs = process.getBuiltinModule("node:fs");
const args = process.argv.slice(2);

function readInput(): string | undefined {
  try {
    // 0 is the standard input. / 0 は標準入力
    return fs.readFileSync(args[0] ?? 0, "utf8");
  } catch (e) {
    process.stderr.write(`wordfreq: ${args[0]}: no such file\n`);
    process.exitCode = 1;
    return undefined;
  }
}

const text = readInput();
if (text !== undefined) {
  const counts = new Map<string, number>();
  for (const w of text.toLowerCase().match(/\p{L}+/gu) ?? []) {
    counts.set(w, (counts.get(w) ?? 0) + 1);
  }
  [...counts]
    .sort(([a, m], [b, n]) => n - m || (a < b ? -1 : 1))
    .slice(0, 10)
    .forEach(([w, n]) => console.log(w.padEnd(12) + n));
}
// ANCHOR_END: tsBody
