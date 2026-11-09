// My own attributes with Zod: a schema built at run time from the
// definitions, for the chapter "Attributes and states of my own".
// 「自分で決める属性と状態」の章の、自分で決めた属性を Zod で：
// 定義から、動かしながらスキーマを組み立てる
import { z } from "zod";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: zod
// TypeScript (Zod: a schema made from data)
// TypeScript（Zod：データからスキーマを作る）

// 🧩 The kinds and the definitions, as plain data (from JSON, say).
// 種類と定義。ただのデータ（JSON から読んだものなど）
type Kind =
  | { kind: "text" }
  | { kind: "number" }
  | { kind: "date" }
  | { kind: "yesno" }
  | { kind: "choice"; choices: [string, ...string[]] };
type AttrDef = { name: string; kind: Kind };

// 🎛️ A Zod schema for each kind. Form fields come as text.
// 種類ごとの Zod のスキーマ。フォームの欄は文字で来る
function toZod(kind: Kind): z.ZodType {
  switch (kind.kind) {
    case "text":   return z.string().trim().min(1);
    case "number": return z.string().regex(/^-?\d+$/).transform(Number);
    case "date":   return z.iso.date();
    case "yesno":
      return z.enum(["はい", "いいえ"]).transform((s) => s === "はい");
    case "choice": return z.enum(kind.choices);
  }
}

// 🧱 One object schema: a key for each definition, nothing else.
// オブジェクトのスキーマを1つ：定義ごとにキーが1つ。ほかのキーは断る
function formSchema(defs: AttrDef[]) {
  const shape = Object.fromEntries(
    defs.map((d) => [d.name, toZod(d.kind).optional()]),
  );
  return z.object(shape).strict();
}

const defs: AttrDef[] = [
  { name: "推し度", kind: { kind: "number" } },
  { name: "買った日", kind: { kind: "date" } },
  { name: "形", kind: { kind: "choice", choices: ["紙", "電子", "Web"] } },
];
const result = formSchema(defs).safeParse({
  推し度: "すごく",
  買った日: "11月1日",
  サイン: "はい",
});
// result.error.issues: every problem, three of them / まずい所が3つとも
// ANCHOR_END: zod

check(!result.success, "refused");
const issues = result.error?.issues ?? [];
check(issues.length === 3, `three issues: ${issues.length}`);
check(issues.some((i) => i.code === "unrecognized_keys"), "サイン");

const good = formSchema(defs).safeParse({ 推し度: "4", 形: "Web" });
check(good.success, "good form");
// ANCHOR: infer
// 🤷 The type of the result: the compiler cannot know 推し度 is a number,
//    because the keys are only known at run time.
// 結果の型：コンパイラーには、推し度が数だとはわからない。
// キーは、動かしてみるまでわからないから
const data: Record<string, unknown> = good.data ?? {};
// ANCHOR_END: infer
check(data["推し度"] === 4 && data["形"] === "Web", JSON.stringify(data));
console.log("ok");
