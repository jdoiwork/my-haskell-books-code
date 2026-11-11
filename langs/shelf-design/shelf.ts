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
  // TypeScript (procedural style: a plain number and style names)
  // TypeScript（手続き型スタイル：ただの数と、見せ方の名前の文字列）

  // 💯 Check a score: a whole number from 0 to 100.
  // 点を確かめる：0〜100 の整数
  function makeScore(n: number): number {
    if (!Number.isInteger(n) || n < 0 || n > 100) {
      throw new Error(`bad score: ${n}`);
    }
    return n;
  }

  // 🖼️ Show a score: "hundred", "stars", "ten" or "ten1" (7.3/10).
  // 点を見せる："hundred"、"stars"、"ten"、"ten1"（7.3/10）
  function render(score: number, style: string): string {
    if (style === "hundred") {
      return `${score}点`;
    } else if (style === "stars") {
      const halves = Math.floor((score + 5) / 10); // halves go up
      if (halves % 2 === 0) {
        return `★${halves / 2}`;
      } else {
        return `★${(halves - 1) / 2}.5`;
      }
    } else if (style === "ten") {
      return `${Math.floor((score + 5) / 10)}/10`;
    } else if (style === "ten1") {
      return `${Math.floor(score / 10)}.${score % 10}/10`;
    } else {
      throw new Error(`bad style: ${style}`);
    }
  }
  // ANCHOR_END: procedural

  // ANCHOR: moves
  // TypeScript (procedural style: the map of reading as an object)
  // TypeScript（手続き型スタイル：読書の地図をオブジェクトで）

  // 🗺️ The map of reading: "state event" -> next state.
  // 読書の地図："状態 できごと" → 次の状態
  const moves: { [key: string]: string } = {
    "piled start": "reading",
    "reading finish": "finished",
    "reading stop": "paused",
    "paused resume": "reading",
    "finished reread": "reading",
  };

  function advance(event: string, state: string): string {
    const next = moves[`${state} ${event}`];
    if (next === undefined) {
      throw new Error(`not allowed: ${event} from ${state}`);
    }
    return next;
  }
  // ANCHOR_END: moves

  check(makeScore(73) === 73, "73");
  check([-1, 101, 7.5].every((n) => refused(() => makeScore(n))), "bad");
  const shown = ["hundred", "stars", "ten", "ten1"].map((s) => render(73, s));
  check(shown.join() === "73点,★3.5,7/10,7.3/10", "73 in four styles");
  check(render(75, "stars") === "★4", "75 stars");
  check(render(25, "stars") === "★1.5", "25 stars");
  check(render(4, "stars") === "★0", "4 stars");
  check(render(75, "ten") === "8/10", "75 ten");
  check(render(80, "ten1") === "8.0/10", "80 ten1");
  // A typo in a style is not caught until it runs.
  // 見せ方の打ちまちがいは、動かすまでわからない
  check(refused(() => render(73, "starz")), "typo style");
  // Math.round goes up at halves, even for -2.5 (it gives -2).
  // Math.round は、真ん中を大きいほうへ。-2.5 でも（-2 になる）
  check(Math.round(2.5) === 3 && Math.round(-2.5) === -2, "Math.round");
  check(advance("start", "piled") === "reading", "start");
  check(advance("reread", "finished") === "reading", "reread");
  check(refused(() => advance("start", "finished")), "start finished");
  // A typo in a state is not caught until it runs.
  // 状態の打ちまちがいは、動かすまでわからない
  check(refused(() => advance("start", "pilled")), "typo");
}

{
  // ANCHOR: functional
  // TypeScript (functional style: a branded Score and a union of styles)
  // TypeScript（関数型スタイル：ブランド付きの Score と、見せ方の union）
  type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: string };

  // 💯 The model: a number that has passed the check.
  // モデル：確かめを通った数
  type Score = number & { readonly __brand: "Score" };

  function makeScore(n: number): Result<Score> {
    return Number.isInteger(n) && n >= 0 && n <= 100
      ? { ok: true, value: n as Score }
      : { ok: false, error: `bad score: ${n}` };
  }

  // 🎨 Ways to show a score. None of them is the score itself.
  // 点の見せ方。どれも、点そのものではない
  type RatingStyle =
    | { kind: "hundred" }                        // 73点
    | { kind: "stars" }                          // ★3.5
    | { kind: "outOfTen"; decimals: boolean };   // 7/10, 7.3/10
  // ANCHOR_END: functional

  // ANCHOR: render
  // TypeScript (functional style: an exhaustive switch)
  // TypeScript（関数型スタイル：もれのない switch）

  // 📏 How many points one step of the style is worth.
  // その見せ方の1目盛りが、何点ぶんか
  function step(style: RatingStyle): number {
    switch (style.kind) {
      case "hundred": return 1;
      case "stars": return 10;
      case "outOfTen": return style.decimals ? 1 : 10;
    }
  }

  // 🖼️ Show a score; a coarse style rounds halves up (75 is ★4).
  // 点を見せる。粗い見せ方は、真ん中を切り上げる（75 点は★4）
  function render(style: RatingStyle, score: Score): string {
    const steps = Math.floor((score + step(style) / 2) / step(style));
    switch (style.kind) {
      case "hundred": return `${score}点`;
      case "stars": return `★${steps / 2}`;   // 3.5, or 4 (not 4.0)
      case "outOfTen":
        return style.decimals
          ? `${Math.floor(score / 10)}.${score % 10}/10`
          : `${steps}/10`;
      default:
        // A new style with no case makes this line an error.
        // case のない見せ方を足すと、この行がエラーになる
        const _exhaustive: never = style;
        return _exhaustive;
    }
  }
  // ANCHOR_END: render

  // ANCHOR: parse
  // TypeScript (functional style: read typed text back into the model)
  // TypeScript（関数型スタイル：打ちこまれた文字を、モデルに読み戻す）
  const marks: Record<RatingStyle["kind"], RegExp> = {
    hundred: /点$/,
    stars: /^★/,
    outOfTen: /\/10$/,
  };

  // 📥 Read text in a style back into a score. Marks may be left out.
  // その見せ方の文字を、点に読み戻す。印は省いてもよい
  function parse(style: RatingStyle, text: string): Result<Score> {
    const bad: Result<Score> = { ok: false, error: `bad rating: ${text}` };
    const good = (n: number): Result<Score> =>
      ({ ok: true, value: n as Score });
    const number = text.trim().replace(marks[style.kind], "").trim();
    const m = /^([0-9]{1,4})(?:\.([0-9]))?$/.exec(number);
    if (m === null) return bad;
    const t = Number(m[1]) * 10 + Number(m[2] ?? 0);  // tenths / 0.1 の数
    switch (style.kind) {
      case "hundred":
        return t % 10 === 0 && t <= 1000 ? good(t / 10) : bad;
      case "stars":
        return t % 5 === 0 && t <= 50 ? good(t * 2) : bad;
      case "outOfTen":
        return (style.decimals || t % 10 === 0) && t <= 100 ? good(t) : bad;
    }
  }
  // ANCHOR_END: parse

  const allStyles: RatingStyle[] = [
    { kind: "hundred" },
    { kind: "stars" },
    { kind: "outOfTen", decimals: false },
    { kind: "outOfTen", decimals: true },
  ];
  const [hundred, stars, ten, ten1] = allStyles;
  const must = (r: Result<Score>): Score => {
    if (!r.ok) throw new Error(r.error);
    return r.value;
  };
  const s = (n: number) => must(makeScore(n));
  check(s(73) === 73, "73");
  check([-1, 101, 7.5].every((n) => !makeScore(n).ok), "bad scores");
  const shown = allStyles.map((st) => render(st, s(73))).join();
  check(shown === "73点,★3.5,7/10,7.3/10", "73 in four styles");
  check(render(stars, s(75)) === "★4", "75 stars");
  check(render(stars, s(25)) === "★1.5", "25 stars");
  check(render(stars, s(4)) === "★0", "4 stars");
  check(render(stars, s(100)) === "★5", "100 stars");
  check(render(ten, s(75)) === "8/10", "75 ten");
  check(render(ten1, s(80)) === "8.0/10", "80 ten1");
  check(must(parse(stars, "★4.5")) === 90, "★4.5");
  check(must(parse(stars, " 4 ")) === 80, "4");
  check(must(parse(hundred, "73点")) === 73, "73点");
  check(must(parse(ten1, "7.5")) === 75, "7.5");
  check(must(parse(ten1, "8.0/10")) === 80, "8.0/10");
  check(must(parse(ten, "7/10")) === 70, "7/10");
  const bad: [RatingStyle, string][] = [
    [stars, "★6"], [stars, "★3.3"], [ten, "7.5"], [ten1, "10.1"],
    [hundred, "101"], [hundred, "7.5"], [hundred, "-3"],
    [hundred, "７３"], [stars, "3.25"], [hundred, ""],
  ];
  for (const [st, text] of bad) {
    const r = parse(st, text);
    check(!r.ok && r.error.startsWith("bad rating"), `${st.kind} ${text}`);
  }
  for (const st of allStyles) {
    for (let n = 0; n <= 100; n++) {
      const back = must(parse(st, render(st, s(n))));
      // A shadow is near the model; on a step, it is the model itself.
      // 影はモデルの近く。目盛りの上なら、モデルそのもの
      check(Math.abs(back - n) <= step(st) / 2, `near ${st.kind} ${n}`);
      check(n % step(st) !== 0 || back === n, `same ${st.kind} ${n}`);
    }
  }
  // tsc stops these lines (not run).
  // これらの行は tsc が止める（動かさない）
  const typeErrors = () => {
    // A plain number is not a Score. / ただの数は Score ではない
    // @ts-expect-error
    render(hundred, 73);
    // @ts-expect-error A typo in a style. / 見せ方の打ちまちがい
    render({ kind: "starz" }, s(73));
    // outOfTen needs decimals. / outOfTen には decimals が要る
    // @ts-expect-error
    render({ kind: "outOfTen" }, s(73));
  };
  void typeErrors;
}

{
  type Result<T> =
    | { ok: true; value: T }
    | { ok: false; error: string };

  // ANCHOR: states
  // TypeScript (functional style: union types in the map of reading)
  // TypeScript（関数型スタイル：読書の地図に union の型）
  type State = "piled" | "reading" | "finished" | "paused";
  type Event = "start" | "finish" | "stop" | "resume" | "reread";

  // 🗺️ Every state must have a line; a misspelt one is an error.
  // どの状態にも行が要る。つづりをまちがえるとエラー
  const moves: Record<State, Partial<Record<Event, State>>> = {
    piled: { start: "reading" },
    reading: { finish: "finished", stop: "paused" },
    paused: { resume: "reading" },
    finished: { reread: "reading" },
  };

  // ➡️ Take one step, or say why not.
  // 1歩進む。進めなければ、そのわけを言う
  function advance(event: Event, state: State): Result<State> {
    const next = moves[state][event];
    return next === undefined
      ? { ok: false, error: `not allowed: ${event} from ${state}` }
      : { ok: true, value: next };
  }
  // ANCHOR_END: states

  check(!advance("start", "finished").ok, "start finished");
  const r = advance("reread", "finished");
  check(r.ok && r.value === "reading", "reread");
  const typeErrors = () => {
    // A typo in a state. / 状態の打ちまちがい
    // @ts-expect-error
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
    | { kind: "series"; toc: Entry[] }   // seasons: minutes / シーズン：分
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
  // A streaming drama: seasons and episodes. / 配信ドラマ：シーズンと話
  const detective: Work = {
    kind: "series",
    toc: [
      { heading: "シーズン1 第1話 消えた写本", size: 52 },
      { heading: "シーズン1 第2話 閉館後の足音", size: 48 },
      { heading: "シーズン1 第3話 司書は知っていた", size: 55 },
      { heading: "シーズン2 第1話 帰ってきた探偵", size: 50 },
    ],
  };
  check(length(detective) === "205 minutes", "series");
}
