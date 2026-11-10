// ANCHOR: all
// The API, called with the types made from Haskell (tsundoku-api.ts).
// The same shape as the client in the API chapter, with every route the
// screen uses. Every call answers an Answer: the value, or a Problem.
// API を、Haskell から作った型（tsundoku-api.ts）で呼ぶ。API の章の
// クライアントと同じ形で、画面が使う道を全部そろえた。どの呼び出しも
// Answer を返す：値か、Problem か
import type {
  CardDetail,
  EditedWork,
  ExtrasItem,
  FormField,
  HitItem,
  NewWork,
  NoteItem,
  Problem,
  ReadingNow,
  StatsItem,
  TagCount,
  WorkSummary,
} from "./tsundoku-api";

// ANCHOR: answer
export type Answer<T> =
  | { ok: true; value: T }
  | { ok: false; problem: Problem };

async function ask<T>(res: Response): Promise<Answer<T>> {
  if (res.ok) {
    const value = res.status === 204 ? undefined : await res.json();
    return { ok: true, value: value as T };
  }
  return { ok: false, problem: problemOf(res, await res.text()) };
}

// 💬 Our own refusals are a Problem in JSON. Others are not: servant's
//    405 and 415, warp's 500 when a handler throws, the dev server's 502
//    when `shelf serve` is down. Make a Problem of those too.
// 自分で返す断りは、JSON の Problem。ほかはちがう：servant の 405・415、
// ハンドラーが例外で止まったときの warp の 500、shelf serve が止まって
// いるときの開発サーバーの 502。それも Problem にする
function problemOf(res: Response, body: string): Problem {
  try {
    const said = JSON.parse(body);
    if (Array.isArray(said?.messages)) return said as Problem;
  } catch {
    // Not JSON: say the status and the words as they came.
    // JSON ではない：状態と、届いた言葉をそのまま言う
  }
  const words = `${res.status} ${res.statusText} ${body}`.trim();
  return { mistake: "not-a-problem", messages: [words] };
}
// ANCHOR_END: answer

// ANCHOR: routes
// 🧭 One function for each route. Vite sends /api to `shelf serve`.
// 道ごとに関数1つ。/api は、Vite が shelf serve に回す
const base = "/api";
const json = { "Content-Type": "application/json" };
const send = (method: string, body: unknown): RequestInit =>
  ({ method, headers: json, body: JSON.stringify(body) });

export const shelf = {
  listWorks: async () =>
    ask<WorkSummary[]>(await fetch(`${base}/works`)),
  card: async (id: number) =>
    ask<CardDetail>(await fetch(`${base}/works/${id}`)),
  addWork: async (work: NewWork) =>
    ask<CardDetail>(await fetch(`${base}/works`, send("POST", work))),
  revise: async (id: number, edited: EditedWork) =>
    ask<CardDetail>(await fetch(`${base}/works/${id}`,
                                send("PUT", edited))),
  form: async (id: number) =>
    ask<FormField[]>(await fetch(`${base}/works/${id}/form`)),
  happen: async (id: number, event: string) =>
    ask<ReadingNow>(await fetch(`${base}/works/${id}/reading`,
                                send("POST", { event }))),
  rate: async (id: number, points: number) =>
    ask<void>(await fetch(`${base}/works/${id}/rating`,
                          send("PUT", { points }))),
  // ANCHOR: marks
  logReading: async (id: number, day: string, entry: number) =>
    ask<void>(await fetch(`${base}/works/${id}/log`,
                          send("POST", { day, entry }))),
  unread: async (id: number, entry: number) =>
    ask<void>(await fetch(`${base}/works/${id}/log/${entry}`,
                          { method: "DELETE" })),
  // ANCHOR_END: marks
  addNote: async (id: number, body: string, spoiler: boolean) =>
    ask<NoteItem>(await fetch(`${base}/works/${id}/notes`,
                              send("POST", { entry: null, body, spoiler }))),
  editAttrs: async (id: number, fields: Record<string, string>) =>
    ask<FormField[]>(await fetch(`${base}/works/${id}/attrs`,
                                 send("PUT", fields))),
  search: async (q: string) =>
    ask<HitItem[]>(await fetch(`${base}/search?q=${encodeURIComponent(q)}`)),
  // ANCHOR: site
  // 🔖 The review site's routes (this chapter).
  // レビューサイトの道（この章）
  extras: async (id: number) =>
    ask<ExtrasItem>(await fetch(`${base}/works/${id}/extras`)),
  setTags: async (id: number, tags: string[]) =>
    ask<string[]>(await fetch(`${base}/works/${id}/tags`,
                              send("PUT", tags))),
  tags: async () =>
    ask<TagCount[]>(await fetch(`${base}/tags`)),
  tagged: async (tag: string) =>
    ask<WorkSummary[]>(
      await fetch(`${base}/tags/${encodeURIComponent(tag)}`)),
  stats: async (year: number) =>
    ask<StatsItem>(await fetch(`${base}/stats?year=${year}`)),
  // ANCHOR_END: site
};
// ANCHOR_END: routes
// ANCHOR_END: all
