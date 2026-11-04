// The TypeScript code in the language design chapter.
// 言語を設計する章の TypeScript のコード

// ANCHOR: union
// TypeScript (a union type with a "kind" tag)
// TypeScript（"kind" の札を持つ union 型）
type Expr =
  | { kind: "lit"; value: number | boolean }
  | { kind: "var"; name: string }
  | { kind: "lam"; param: string; body: Expr }
  | { kind: "app"; fun: Expr; arg: Expr }
  | { kind: "let"; name: string; value: Expr; body: Expr }
  | { kind: "if"; cond: Expr; then: Expr; else: Expr }
  | { kind: "binop"; op: "+" | "-" | "*" | "/" | "==" | "<";
      left: Expr; right: Expr };

function freeVars(e: Expr): Set<string> {
  switch (e.kind) {
    case "lit": return new Set();
    case "var": return new Set([e.name]);
    case "lam": {
      const s = freeVars(e.body);
      s.delete(e.param);
      return s;
    }
    case "app": return new Set([...freeVars(e.fun), ...freeVars(e.arg)]);
    case "let": {
      const s = new Set([...freeVars(e.value), ...freeVars(e.body)]);
      s.delete(e.name);
      return s;
    }
    case "if":
      return new Set([...freeVars(e.cond), ...freeVars(e.then),
                      ...freeVars(e.else)]);
    case "binop":
      return new Set([...freeVars(e.left), ...freeVars(e.right)]);
  }
}
// ANCHOR_END: union

const e: Expr = {
  kind: "lam", param: "x",
  body: { kind: "binop", op: "+",
          left: { kind: "var", name: "x" },
          right: { kind: "var", name: "y" } },
};
const vars = [...freeVars(e)];
if (vars.length !== 1 || vars[0] !== "y") {
  throw new Error("freeVars failed");
}
console.log("ok");
