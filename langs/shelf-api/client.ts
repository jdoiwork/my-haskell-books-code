// The TypeScript side of the shelf's API: calls with the types made from
// Haskell (tsundoku-api.ts), and two small things only a screen does.
// 本棚の API の TypeScript の側：Haskell から作った型（tsundoku-api.ts）で
// 呼ぶ関数と、画面だけがする小さな仕事2つ
import type {
  CardDetail,
  Points,
  Problem,
  ProgressNumbers,
  Snip,
  WorkSummary,
} from "./tsundoku-api.ts";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// ANCHOR: client
// TypeScript (calling the Haskell API with types made from Haskell)
// TypeScript（Haskell から作った型で、Haskell の API を呼ぶ）
type Answer<T> = { ok: true; value: T } | { ok: false; problem: Problem };

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

export function shelfApi(get: typeof fetch, base = "") {
  const json = { "Content-Type": "application/json" };
  return {
    listWorks: async () => ask<WorkSummary[]>(await get(`${base}/works`)),
    card: async (id: number) =>
      ask<CardDetail>(await get(`${base}/works/${id}`)),
    rate: async (id: number, points: Points) =>
      ask<void>(
        await get(`${base}/works/${id}/rating`, {
          method: "PUT",
          headers: json,
          body: JSON.stringify(points),
        }),
      ),
  };
}
// ANCHOR_END: client

// ANCHOR: screen
// TypeScript (what only a screen does: draw the bar, mark the words)
// TypeScript（画面だけがすること：バーを描く、語に印を付ける）
export function bar(p: ProgressNumbers, width = 10): string {
  const filled = Math.floor((width * p.done) / Math.max(p.total, 1));
  return "▓".repeat(filled) + "░".repeat(width - filled) + ` ${p.percent}%`;
}

export function marked(snippet: Snip[]): string {
  return snippet
    .map((s) => (s.match ? `<mark>${s.text}</mark>` : s.text))
    .join("");
}
// ANCHOR_END: screen

// @ts-expect-error: the API takes points, not stars.
const stars: Points = { stars: 4.5 };
void stars;

// A fake fetch that answers like the Haskell server did.
// Haskell のサーバーと同じように答える、にせの fetch
const answers: Record<string, [number, unknown]> = {
  "/works": [200, [{
    id: 1, title: "はじめてのドラゴン学", creator: "ギルド図書室",
    kind: "book", reading: "Reading", rating: 90,
    progress: { done: 48, total: 78, unit: "pages", percent: 62 },
  }]],
  "/works/9": [404, { mistake: "no-such-work", messages: ["本棚にない作品です"] }],
  "PUT /works/1/rating": [422, { mistake: "bad-score", messages: ["点は 0〜100 です：101"] }],
  // the dev server, when shelf serve is down: 502, and no body
  // shelf serve が止まっているときの開発サーバー：502 で、本体なし
  "/works/7": [502, ""],
};
const fake: typeof fetch = async (url, init) => {
  const key = init?.method ? `${init.method} ${url}` : String(url);
  const [status, body] = answers[key] ?? [500, null];
  const text = typeof body === "string" ? body : JSON.stringify(body);
  return new Response(text, { status });
};

const api = shelfApi(fake);
const listed = await api.listWorks();
check(listed.ok && listed.value[0].title === "はじめてのドラゴン学", "list");
if (listed.ok && listed.value[0].progress) {
  check(bar(listed.value[0].progress) === "▓▓▓▓▓▓░░░░ 62%", "bar");
}
const missing = await api.card(9);
check(!missing.ok && missing.problem.mistake === "no-such-work", "404");
const tooMuch = await api.rate(1, { points: 101 });
check(!tooMuch.ok && tooMuch.problem.mistake === "bad-score", "422");
const down = await api.card(7);
check(!down.ok && down.problem.messages[0] === "502", "not JSON");
check(
  marked([{ text: "ドラゴンは", match: false }, { text: "怠けもの", match: true }])
    === "ドラゴンは<mark>怠けもの</mark>",
  "marked",
);
