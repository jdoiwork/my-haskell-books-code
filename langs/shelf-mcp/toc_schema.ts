// The table of contents an AI sends, as zod schemas, for comparing with
// the hand-written JSON Schema of the Haskell MCP server. zod 4 makes the
// JSON Schema from the schema (z.toJSONSchema), and the type from it.
// AI が送る目次を zod のスキーマで。Haskell の MCP サーバーの、手で書いた
// JSON Schema とくらべるため。zod 4 は、スキーマから JSON Schema を作り
// （z.toJSONSchema）、型も作る
import { z } from "zod";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: schema
// TypeScript (zod: a schema that refers to itself through a getter)
// TypeScript（zod：getter で自分を指すスキーマ）
const Chapter = z.object({
  heading: z.string().min(1),
  page: z.number().int().min(1),
  get children() {
    return z.array(Chapter).default([]);
  },
});

const TocDraft = z.object({
  title: z.string(),
  creator: z.string().default(""),
  last_page: z.number().int().min(1),
  toc: z.array(Chapter),
});
type TocDraft = z.infer<typeof TocDraft>;

const jsonSchema = z.toJSONSchema(TocDraft, { io: "input" });
// ANCHOR_END: schema

const text = JSON.stringify(jsonSchema);
check(text.includes('"$ref"'), "the chapter refers to itself");
check(text.includes('"minimum":1'), "pages start at 1");

// What zod says about a bad tree: places as paths of keys and numbers.
// まずい木に zod が言うこと：場所は、キーと番号の道
const bad = TocDraft.safeParse({
  title: "竜の飼い方入門",
  last_page: 120,
  toc: [{ heading: "第1章 卵", page: 1, children: [{ heading: "", page: 3 }] }],
});
check(!bad.success, "an empty heading is refused");
if (!bad.success) {
  const paths = bad.error.issues.map((i) => i.path.join("."));
  check(paths[0] === "toc.0.children.0.heading", `path: ${paths[0]}`);
}

const good: TocDraft = TocDraft.parse({
  title: "x", last_page: 9, toc: [{ heading: "a", page: 1 }],
});
check(good.toc[0].children.length === 0, "children default to []");
console.log("ok");
