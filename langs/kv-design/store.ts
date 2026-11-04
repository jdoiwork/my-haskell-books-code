// The kinds of values in TypeScript, for the chapter
// "Design: values and commands as types".
// 「設計：値とコマンドを型で描く」の章の値の種類を、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: union
// TypeScript (a discriminated union)
// TypeScript（判別可能なユニオン）
type Value =
  | { kind: "string"; text: string }
  | { kind: "list"; items: string[] }
  | { kind: "set"; members: Set<string> }
  | { kind: "hash"; fields: Map<string, string> };

function typeName(v: Value): string {
  switch (v.kind) {
    case "string": return "string";
    case "list":   return "list";
    case "set":    return "set";
    case "hash":   return "hash";
    default: {
      // If a kind is forgotten, v is not `never` and tsc stops here.
      // 種類を書き忘れると、v が never にならず、tsc がここで止める
      const forgotten: never = v;
      return forgotten;
    }
  }
}
// ANCHOR_END: union

check(typeName({ kind: "list", items: ["a"] }) === "list", "list");
check(typeName({ kind: "string", text: "x" }) === "string", "string");
console.log("ok");
