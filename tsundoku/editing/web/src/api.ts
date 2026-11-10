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
  FormField,
  HitItem,
  NewWork,
  NoteItem,
  Problem,
  ReadingNow,
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
  return { ok: false, problem: (await res.json()) as Problem };
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
};
// ANCHOR_END: routes
// ANCHOR_END: all
