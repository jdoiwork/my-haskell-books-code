// The TODO API of the chapter, in Deno (TypeScript). The handler is a
// function from a Request to a Response, so the check calls it as it
// is, with no port. check-langs runs it with no flags: Deno.serve is
// stopped first, and says what flag it needs.
// 章の TODO の API を Deno で。handler は Request から Response への関数
// なので、確かめはポートなしで、そのまま呼ぶ。check-langs はフラグなしで
// 動かすので、Deno.serve はまず止められ、要るフラグを言う
// ANCHOR: handler
// deno run --allow-net=0.0.0.0:8000 todo.deno.ts
type Todo = { id: number; title: string; done: boolean };
const todos: Todo[] = [];

// 🚏 One function: the method and the path choose what to do.
//    URLPattern reads ":id", as Lam's Route does.
// 関数1つ：メソッドと道で、することを選ぶ。URLPattern は ":id" を読む
// （Lam の Route と同じ）
const done = new URLPattern({ pathname: "/todos/:id/done" });

async function handler(req: Request): Promise<Response> {
  const url = new URL(req.url);
  if (req.method === "GET" && url.pathname === "/todos") {
    return Response.json(todos);
  }
  if (req.method === "POST" && url.pathname === "/todos") {
    const { title } = await req.json();
    const todo = { id: todos.length + 1, title, done: false };
    todos.push(todo);
    return Response.json(todo, { status: 201 });
  }
  const found = done.exec(url);
  if (req.method === "PUT" && found) {
    const todo = todos.find((t) => t.id === Number(found.pathname.groups.id));
    if (!todo) return new Response("not found\n", { status: 404 });
    todo.done = true;
    return Response.json(todo);
  }
  return new Response("not found\n", { status: 404 });
}
// ANCHOR_END: handler

// Stop with an error when a check fails (console.assert only prints).
// 確かめが外れたら、エラーで止まる（console.assert は書くだけ）
function check(ok: boolean, ...what: unknown[]): void {
  if (!ok) throw new Error(`check failed: ${JSON.stringify(what)}`);
}

// The check (1): with no flags, Deno.serve may not listen.
// 確かめ（1）：フラグなしでは、Deno.serve は待てない
let refused = "";
try {
  Deno.serve({ port: 8000 }, handler);
} catch (e) {
  refused = `${(e as Error).name}: ${(e as Error).message}`;
}
check(refused.startsWith('NotCapable: Requires net access to "0.0.0.0:8000"'),
  refused);

// The check (2): the handler, called as a function.
// 確かめ（2）：handler を、関数として呼ぶ
const base = "http://localhost:8000";
const post = (title: string) =>
  handler(new Request(`${base}/todos`, {
    method: "POST",
    body: JSON.stringify({ title }),
  }));
const first = await post("ふりかえりを書く");
check(first.status === 201);
check((await first.json()).id === 1);
await post("絵を描く");
const put = await handler(new Request(`${base}/todos/1/done`, {
  method: "PUT",
}));
check((await put.json()).done === true);
const all = await (await handler(new Request(`${base}/todos`))).json();
check(all.length === 2 && all[0].done && !all[1].done, all);
const none = await handler(new Request(`${base}/todos/9/done`, {
  method: "PUT",
}));
check(none.status === 404);
console.log("ok");
