// The TypeScript code in the matcher chapter of MiniRegex: the same tiny
// matcher, written with continuations (callbacks).
// MiniRegex のマッチャーの章の TypeScript のコード：同じ小さなマッチャーを、
// 継続（コールバック）で書いたもの
function same(actual: unknown, expected: unknown): void {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(actual)}`);
  }
}

// ANCHOR: first
// TypeScript: the left branch wins here, too
// TypeScript：ここでも、左の枝が勝つ
console.log("abc".match(/a|ab/)?.[0]);
// ANCHOR_END: first

same("abc".match(/a|ab/)?.[0], "a");

// ANCHOR: continuation
// TypeScript: each part gets the rest of the work, and calls it from
//   where it ends. null is "it did not work".
// TypeScript：どの部分も残りの仕事を受け取り、自分が終わった場所から
//   呼ぶ。null は「うまくいかなかった」
type Rest = (i: number) => number | null;
type Tree =
  | { kind: "lit"; char: string }
  | { kind: "seq"; parts: Tree[] }
  | { kind: "alt"; branches: Tree[] }
  | { kind: "star"; body: Tree };

function run(t: Tree, s: string, i: number, k: Rest): number | null {
  switch (t.kind) {
    case "lit":
      return s[i] === t.char ? k(i + 1) : null;
    case "seq": {
      const [first, ...others] = t.parts;
      if (first === undefined) return k(i);
      const rest: Tree = { kind: "seq", parts: others };
      return run(first, s, i, (j) => run(rest, s, j, k));
    }
    case "alt":
      for (const b of t.branches) {
        const end = run(b, s, i, k);
        if (end !== null) return end;
      }
      return null;
    case "star": {
      const again: Rest = (j) => (j === i ? k(j) : run(t, s, j, k));
      const more = run(t.body, s, i, again);
      return more ?? k(i);
    }
  }
}

const lit = (char: string): Tree => ({ kind: "lit", char });
// (a|ab)*c
const tree: Tree = {
  kind: "seq",
  parts: [
    { kind: "star", body: {
      kind: "alt",
      branches: [lit("a"), { kind: "seq", parts: [lit("a"), lit("b")] }],
    } },
    lit("c"),
  ],
};
console.log(run(tree, "ababc", 0, (i) => i));
// ANCHOR_END: continuation

same(run(tree, "ababc", 0, (i) => i), 5);
same(run(tree, "abab", 0, (i) => i), null);
