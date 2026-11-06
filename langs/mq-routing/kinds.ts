// The TypeScript code in the "routing" chapter of the message broker part.
// メッセージブローカーの部の「仕分け」の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// 🔀 "*" is exactly one word, "#" is any number of words.
// 「*」はちょうど1語、「#」は何語でも
function topicMatch(ps: string[], ws: string[]): boolean {
  if (ps.length === 0) return ws.length === 0;
  const [p, ...rest] = ps;
  if (p === "#") {
    return ws.some((_, i) => topicMatch(rest, ws.slice(i))) ||
      topicMatch(rest, []);
  }
  if (ws.length === 0) return false;
  return (p === "*" || p === ws[0]) && topicMatch(rest, ws.slice(1));
}

// ANCHOR: union
// TypeScript (a union of strings, and a switch that checks every kind)
// TypeScript（文字列の union 型と、全部の種類を確かめる switch）
type ExchangeKind = "fanout" | "direct" | "topic";

// 🗂️ Does a binding with this pattern take a letter with this key?
// このパターンの結び付けは、このキーの手紙を受け取るか
function matches(
  kind: ExchangeKind,
  pattern: string,
  key: string,
): boolean {
  switch (kind) {
    case "fanout":
      return true;
    case "direct":
      return pattern === key;
    case "topic":
      return topicMatch(pattern.split("."), key.split("."));
    default: {
      // Add a kind and forget a case: this line stops compiling.
      // 種類を足して case を忘れると、この行がコンパイルできなくなる
      const forgotten: never = kind;
      return forgotten;
    }
  }
}
// ANCHOR_END: union

check(matches("fanout", "", "quest.dragon.north"), "fanout");
check(matches("direct", "dragon", "dragon"), "direct");
check(!matches("direct", "dragon", "herb"), "direct miss");
check(matches("topic", "quest.*", "quest.dragon"), "star");
check(!matches("topic", "quest.*", "quest"), "star needs a word");
check(matches("topic", "quest.#", "quest"), "hash zero");
check(matches("topic", "quest.#", "quest.dragon.north"), "hash many");
check(matches("topic", "quest.*.north", "quest.herb.north"), "middle");
check(
  !matches("topic", "quest.dragon.*", "quest.dragon.north.cave"),
  "too long",
);
console.log("ok");
