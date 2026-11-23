// ANCHOR: all
// 🎨 Color test/sample.lam with the grammar, as VS Code does, and
//    compare the words and their colors (scopes) with
//    test/sample.tokens.
// VS Code と同じように、文法で test/sample.lam に色を付け、語と色
// （スコープ）を test/sample.tokens とくらべる
import { readFileSync, writeFileSync } from "node:fs";
import * as oniguruma from "vscode-oniguruma";
import * as textmate from "vscode-textmate";

const here = __dirname + "/../..";

async function main(): Promise<void> {
  // The regular expressions of TextMate are Oniguruma's (in wasm).
  // TextMate の正規表現は Oniguruma のもの（wasm で動かす）
  const wasm = readFileSync(
    require.resolve("vscode-oniguruma/release/onig.wasm"),
  );
  await oniguruma.loadWASM(wasm.buffer);
  const registry = new textmate.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (ps: string[]) => new oniguruma.OnigScanner(ps),
      createOnigString: (s: string) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async () =>
      textmate.parseRawGrammar(
        readFileSync(here + "/syntaxes/lam.tmLanguage.json", "utf8"),
        "lam.tmLanguage.json",
      ),
  });
  const grammar = await registry.loadGrammar("source.lam");
  if (grammar === null) throw new Error("no grammar");
  // ANCHOR: tokenize
  // Line by line: each word, with its innermost scope.
  // 1行ずつ：語ごとに、いちばん内側のスコープ
  const lines = readFileSync(here + "/test/sample.lam", "utf8")
    .trimEnd()
    .split("\n");
  let state = textmate.INITIAL;
  const out: string[] = [];
  for (const line of lines) {
    const r = grammar.tokenizeLine(line, state);
    out.push(line);
    for (const t of r.tokens) {
      const word = line.slice(t.startIndex, t.endIndex);
      const scope = t.scopes[t.scopes.length - 1];
      if (word.trim() !== "" && scope !== "source.lam") {
        out.push(`  ${JSON.stringify(word)} ${scope}`);
      }
    }
    state = r.ruleStack;
  }
  // ANCHOR_END: tokenize
  const got = out.join("\n") + "\n";
  writeFileSync(here + "/test/actual.tokens", got);
  const want = readFileSync(here + "/test/sample.tokens", "utf8");
  if (got !== want) {
    console.error("the colors differ: see test/actual.tokens");
    process.exit(1);
  }
  console.log(`${lines.length} lines: the colors are as expected`);
}

main();
// ANCHOR_END: all
