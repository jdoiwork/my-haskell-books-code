// An MCP server with the official TypeScript SDK, for comparing with the
// chapter's Haskell server. One tool takes the table of contents an AI
// sends; the SDK makes the tool's JSON Schema from the zod schema. The
// check runs a client in the same process (InMemoryTransport), so no
// network and no AI are needed.
// 公式の TypeScript の SDK で書いた MCP のサーバー。章の Haskell のサーバー
// とくらべるため。道具は1つで、AI が送る目次を受け取る。道具の JSON Schema
// は、SDK が zod のスキーマから作る。確かめは、同じプロセスのクライアント
// （InMemoryTransport）で行うので、ネットも AI も要らない
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { InMemoryTransport } from "@modelcontextprotocol/sdk/inMemory.js";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: server
// TypeScript (the MCP SDK: a tool from a zod schema)
// TypeScript（MCP の SDK：zod のスキーマから道具を作る）
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { z } from "zod";

const Chapter = z.object({
  heading: z.string().min(1).describe("見出し（空にしない）"),
  page: z.number().int().min(1).describe("始まりのページ"),
  get children() {
    return z.array(Chapter).default([]);
  },
});
type Chapter = z.infer<typeof Chapter>;

const server = new McpServer({ name: "shelf", version: "0.1.0" });

server.registerTool(
  "check_toc",
  {
    description: "AI が写した目次を確かめる（登録はしない）",
    inputSchema: {
      title: z.string().describe("本の題"),
      last_page: z.number().int().min(1).describe("最後のページ"),
      toc: z.array(Chapter),
    },
  },
  async ({ last_page, toc }) => {
    // The rule between items is ours to write, as in the chapter.
    // 項目どうしの決まりは、章と同じく、自分で書く
    const problems = pageProblems(toc, last_page);
    return problems.length === 0
      ? { content: [{ type: "text", text: "よさそうです" }] }
      : { content: [{ type: "text", text: problems.join("\n") }],
          isError: true };
  },
);

// 📖 Pages must not go back, nor past the last page.
// ページは戻らず、最後のページを超えない
function pageProblems(toc: Chapter[], last: number): string[] {
  const flat: Chapter[] = [];
  const walk = (cs: Chapter[]) =>
    cs.forEach((c) => { flat.push(c); walk(c.children); });
  walk(toc);
  return flat.flatMap((c, i) => [
    ...(c.page > last ? [`「${c.heading}」が本の外（${c.page}）`] : []),
    ...(i > 0 && c.page < flat[i - 1].page
      ? [`「${c.heading}」が前より前のページ（${c.page}）`]
      : []),
  ]);
}
// ANCHOR_END: server

// ANCHOR: client
// TypeScript (a client in the same process asks the server)
// TypeScript（同じプロセスのクライアントが、サーバーに聞く）
const [near, far] = InMemoryTransport.createLinkedPair();
await server.connect(far);
const client = new Client({ name: "try", version: "0.1.0" });
await client.connect(near);

const { tools } = await client.listTools();
console.log(JSON.stringify(tools[0].inputSchema.required));
// ["title","last_page","toc"]

const answer = await client.callTool({
  name: "check_toc",
  arguments: {
    title: "竜の飼い方入門",
    last_page: 120,
    toc: [
      { heading: "第1章 卵", page: 1 },
      { heading: "第2章 ひな", page: 40 },
      { heading: "第3章 空へ", page: 12 },
    ],
  },
});
console.log(answer.isError, answer.content);
// true [ { type: 'text', text: '「第3章 空へ」が前より前のページ（12）' } ]
// ANCHOR_END: client

check(
  JSON.stringify(tools[0].inputSchema.required) ===
    JSON.stringify(["title", "last_page", "toc"]),
  "the schema says what is required",
);
check(answer.isError === true, "pages that go back are refused");
check(
  JSON.stringify(answer.content).includes("前より前のページ（12）"),
  "the answer names the heading",
);

// A heading that is empty does not reach our function: the SDK refuses it
// with the zod schema first.
// 空の見出しは、自分の関数まで来ない。SDK が zod のスキーマで先に断る
const empty = await client.callTool({
  name: "check_toc",
  arguments: {
    title: "竜の飼い方入門",
    last_page: 120,
    toc: [{ heading: "", page: 1 }],
  },
});
check(empty.isError === true, "an empty heading is refused by the SDK");

await client.close();
await server.close();
console.log("ok");
