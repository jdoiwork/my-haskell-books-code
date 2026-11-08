// The bookshelf's types in TypeScript, for the chapter
// "Design: works, contents, notes and ratings as types".
// 「設計：作品・目次・感想・評価を型で描く」の章の、本棚の型を TypeScript で

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

function refused(f: () => unknown): boolean {
  try {
    f();
    return false;
  } catch {
    return true;
  }
}

// Each part lives in its own block, so the names can be reused.
// 部分ごとにブロックを分けて、同じ名前を使えるようにする
export {};

{
  // ANCHOR: procedural
  // TypeScript (procedural style: plain numbers, strings and an object)
  // TypeScript（手続き型スタイル：ただの数と文字列とオブジェクト）

  // ⭐ Check the stars: 1 to 5, by halves. Keep them as half stars.
  // ★を確かめる：1〜5 で、0.5刻み。半分の★の数で持つ
  function makeStars(x: number): number {
    if (x < 1 || x > 5 || !Number.isInteger(x * 2)) {
      throw new Error(`bad stars: ${x}`);
    }
    return x * 2;
  }

  // 🗺️ The map of reading: "state event" -> next state.
  // 読書の地図："状態 できごと" → 次の状態
  const moves: { [key: string]: string } = {
    "piled start": "reading",
    "reading finish": "finished",
    "reading stop": "paused",
    "paused resume": "reading",
    "finished reread": "rereading",
    "rereading finish": "finished",
    "rereading stop": "finished",
  };

  function advance(event: string, state: string): string {
    const next = moves[`${state} ${event}`];
    if (next === undefined) {
      throw new Error(`not allowed: ${event} from ${state}`);
    }
    return next;
  }
  // ANCHOR_END: procedural

  check(makeStars(3.5) === 7, "3.5 stars");
  check([6, 3.3, -3].every((x) => refused(() => makeStars(x))), "bad");
  check(advance("start", "piled") === "reading", "start");
  check(advance("stop", "rereading") === "finished", "stop rereading");
  check(refused(() => advance("start", "finished")), "start finished");
  // A typo in a state is not caught until it runs.
  // 状態の打ちまちがいは、動かすまでわからない
  check(refused(() => advance("start", "pilled")), "typo");
}

{
  // ANCHOR: functional
  // TypeScript (functional style: a branded type and a Result)
  // TypeScript（関数型スタイル：ブランド付きの型と Result）

  // ⭐ A number that has passed the check. Only makeStars makes one.
  // 確かめを通った数。作れるのは makeStars だけ
  type Stars = number & { readonly __brand: "Stars" };
  type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: string };

  function makeStars(x: number): Result<Stars> {
    if (x < 1 || x > 5 || !Number.isInteger(x * 2)) {
      return { ok: false, error: `bad stars: ${x}` };
    }
    return { ok: true, value: (x * 2) as Stars };
  }

  type State = "piled" | "reading" | "finished" | "paused" | "rereading";
  type Event = "start" | "finish" | "stop" | "resume" | "reread";

  // 🗺️ Every state must have a line; a misspelt one is an error.
  // どの状態にも行が要る。つづりをまちがえるとエラー
  const moves: Record<State, Partial<Record<Event, State>>> = {
    piled: { start: "reading" },
    reading: { finish: "finished", stop: "paused" },
    paused: { resume: "reading" },
    finished: { reread: "rereading" },
    rereading: { finish: "finished", stop: "finished" },
  };

  function advance(event: Event, state: State): Result<State> {
    const next = moves[state][event];
    return next === undefined
      ? { ok: false, error: `not allowed: ${event} from ${state}` }
      : { ok: true, value: next };
  }
  // ANCHOR_END: functional

  // 💯 Stars to a score, and back (round halves up, at least 1 star).
  // ★を点に、点を★に（真ん中は切り上げ、★1 より下にはしない）
  function starsToScore(s: Stars): number {
    return s * 10;
  }
  function scoreToStars(n: number): Stars {
    return Math.min(Math.max(Math.floor((n + 5) / 10), 2), 10) as Stars;
  }

  const ok = makeStars(3.5);
  check(ok.ok && ok.value === 7, "3.5 stars");
  check([6, 3.3, -3].every((x) => !makeStars(x).ok), "bad stars");
  check(scoreToStars(25) === 3, "25 points");
  check(scoreToStars(24) === 2, "24 points");
  for (let h = 2; h <= 10; h++) {
    const s = h as Stars;
    check(scoreToStars(starsToScore(s)) === s, `round trip ${h}`);
  }
  check(!advance("start", "finished").ok, "start finished");
  const r = advance("stop", "rereading");
  check(r.ok && r.value === "finished", "stop rereading");
  // tsc stops these two lines (not run). / この2行は tsc が止める（動かさない）
  const typeErrors = () => {
    // @ts-expect-error A plain number is not Stars. / ただの数は Stars ではない
    starsToScore(7);
    // @ts-expect-error A typo in a state. / 状態の打ちまちがい
    advance("start", "pilled");
  };
  void typeErrors;
}

{
  // ANCHOR: exhaustive
  // TypeScript (a discriminated union and an exhaustive switch)
  // TypeScript（判別できる union と、もれのない switch）
  type Entry = { heading: string; size: number };
  type Work =
    | { kind: "book"; toc: Entry[] }     // pages / ページ
    | { kind: "serial"; toc: Entry[] }   // episodes / 話
    | { kind: "series"; toc: Entry[] }   // minutes / 分
    | { kind: "film"; minutes: number }
    | { kind: "game" };

  const total = (toc: Entry[]) => toc.reduce((n, e) => n + e.size, 0);

  // 📏 How long is it? Each kind answers in its own unit.
  // どれくらいの長さ？種類ごとに、自分の単位で答える
  function length(w: Work): string {
    switch (w.kind) {
      case "book": return `${total(w.toc)} pages`;
      case "serial": return `${total(w.toc)} episodes`;
      case "series": return `${total(w.toc)} minutes`;
      case "film": return `${w.minutes} minutes`;
      case "game": return "no table of contents";
      default:
        // A new kind with no case makes this line an error.
        // case のない種類を足すと、この行がエラーになる
        const _exhaustive: never = w;
        return _exhaustive;
    }
  }
  // ANCHOR_END: exhaustive

  const dragonBook: Work = {
    kind: "book",
    toc: [
      { heading: "第1部 ドラゴンを知る", size: 120 },
      { heading: "第2部 ドラゴンと話す", size: 200 },
    ],
  };
  const lastDragon: Work = { kind: "film", minutes: 121 };
  check(length(dragonBook) === "320 pages", "book");
  check(length(lastDragon) === "121 minutes", "film");
  check(length({ kind: "game" }) === "no table of contents", "game");
}
