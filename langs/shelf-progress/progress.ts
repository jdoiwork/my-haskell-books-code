// The progress of reading in TypeScript, for the chapter
// "The tree of contents and how far you have read".
// 「目次の木と進み具合」の章の、読んだ量の集計を TypeScript で

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`check failed: ${what}`);
}

// Each part lives in its own block, so the names can be reused.
// 部分ごとにブロックを分けて、同じ名前を使えるようにする
export {};

// The sample book, "Dragonology for Beginners", as plain data.
// サンプルの本『はじめてのドラゴン学』を、ただのデータで
type Raw = [number, string, number, Raw[]];
const chapter1: Raw = [2, "第1章 見分け方", 4, [
  [3, "1.1 うろこの色", 8, []],
  [4, "1.2 火を吐くもの", 10, []],
]];
const part1: Raw = [1, "第1部 ドラゴンを知る", 2, [
  chapter1,
  [5, "第2章 すみかと季節", 24, []],
]];
const part2: Raw = [6, "第2部 ドラゴンと話す", 2, [
  [7, "第3章 あいさつの作法", 16, []],
  [8, "第4章 怒らせたときは", 12, []],
]];
const book: Raw[] = [part1, part2];
const read = new Set([1, 2, 3, 4, 5]);
const percents: [number, number, number][] = [
  [48, 78, 62], [1, 1000, 1], [999, 1000, 99], [0, 30, 0],
  [30, 30, 100], [0, 0, 0], [1, 2, 50], [1, 8, 13],
];
// Results to expect: [trees, read, done, total].
// 期待する結果：[木, 読んだ番号, 読んだ量, 全体の量]
const cases: [Raw[], Set<number>, number, number][] = [
  [book, read, 48, 78],
  [[part1], read, 48, 48],
  [[part2], read, 0, 30],
  [[chapter1], read, 22, 22],
  [[], read, 0, 0],
  [[[1, "あとがき", 3, []]], new Set([1]), 3, 3],
  [[[1, "あとがき", 3, []]], new Set(), 0, 3],
  [book, new Set(), 0, 78],
  [book, new Set([1, 2, 3, 4, 5, 6, 7, 8]), 78, 78],
];

{
  // ANCHOR: procedural
  // TypeScript (procedural style: walk the tree with a stack)
  // TypeScript（手続き型スタイル：スタックで木を歩く）

  // 📑 One entry: its own pages (not its children's) and children.
  // 目次の1項目：その項目だけの量（子の量は入らない）と、子の項目
  interface Entry {
    no: number;
    title: string;
    pages: number;
    children: Entry[];
  }

  // 📖 Add up the pages read and all the pages of the contents.
  // 目次の、読んだ量と全体の量を足していく
  function progress(
    entries: Entry[],
    read: Set<number>,
  ): [done: number, total: number] {
    let done = 0;
    let total = 0;
    const stack = [...entries];
    while (stack.length > 0) {
      const entry = stack.pop()!;
      total += entry.pages;
      if (read.has(entry.no)) {
        done += entry.pages;
      }
      for (const child of entry.children) {
        stack.push(child);
      }
    }
    return [done, total];
  }

  // 📊 Percent, rounded half up; 1 to 99 while you are reading.
  // 百分率。四捨五入して、読んでいる途中は 1〜99 におさめる
  function percent(done: number, total: number): number {
    if (total === 0) {
      return 0;
    }
    let p = Math.floor((done * 200 + total) / (2 * total));
    if (done > 0 && p === 0) {
      p = 1; // started: not 0% / 読み始めたら 0% にしない
    }
    if (done < total && p === 100) {
      p = 99; // not finished: not 100% / 読み終わるまで 100% にしない
    }
    return p;
  }
  // ANCHOR_END: procedural

  const toEntry = ([no, title, pages, children]: Raw): Entry =>
    ({ no, title, pages, children: children.map(toEntry) });
  for (const [trees, r, d, t] of cases) {
    const got = progress(trees.map(toEntry), r);
    check(got[0] === d && got[1] === t, `procedural ${d}/${t}`);
  }
  check(percent(48, 78) === 62, "62%");
  for (const [d, t, p] of percents) {
    check(percent(d, t) === p, `percent ${d}/${t}`);
  }
}

{
  // ANCHOR: functional
  // TypeScript (functional style: reduce with add and empty)
  // TypeScript（関数型スタイル：add と empty で reduce する）

  // 📏 Units as branded types: pages and minutes do not mix.
  // 量の単位をブランド付きの型で。ページと分は混ざらない
  type Pages = number & { readonly __brand: "Pages" };
  type Minutes = number & { readonly __brand: "Minutes" };

  // ➕ Progress is a value you can add; empty is where adding starts.
  // 進み具合は足し算できる値。empty は足し始めの値
  type Progress<U extends number> = { done: U; total: U };

  function empty<U extends number>(): Progress<U> {
    return { done: 0 as U, total: 0 as U };
  }

  function add<U extends number>(
    a: Progress<U>,
    b: Progress<U>,
  ): Progress<U> {
    return {
      done: (a.done + b.done) as U,
      total: (a.total + b.total) as U,
    };
  }

  // 📑 One entry, with its own amount in a unit U.
  // 目次の1項目。その項目だけの量を、単位 U で持つ
  type Entry<U extends number> = {
    no: number;
    title: string;
    amount: U;
    children: Entry<U>[];
  };

  // 🌳 Fold a tree: its own progress plus each child's.
  // 木を畳む：自分の進み具合に、子の進み具合を足していく
  function progressOf<U extends number>(
    entry: Entry<U>,
    read: Set<number>,
  ): Progress<U> {
    const done = read.has(entry.no) ? entry.amount : (0 as U);
    const own: Progress<U> = { done, total: entry.amount };
    return entry.children
      .map((child) => progressOf(child, read))
      .reduce(add, own);
  }

  // 📚 Fold all the trees, starting from empty.
  // すべての木を畳む。empty から足し始める
  function progress<U extends number>(
    entries: Entry<U>[],
    read: Set<number>,
  ): Progress<U> {
    return entries
      .map((entry) => progressOf(entry, read))
      .reduce(add, empty<U>());
  }

  // 📊 Percent, rounded half up; 1 to 99 while you are reading.
  // 百分率。四捨五入して、読んでいる途中は 1〜99 におさめる
  function percent<U extends number>({ done, total }: Progress<U>) {
    if (total === 0) return 0;
    const p = Math.floor((done * 200 + total) / (2 * total));
    const reading = 0 < done && done < total;
    return reading ? Math.min(Math.max(p, 1), 99) : p;
  }
  // ANCHOR_END: functional

  const toPages = ([no, title, pages, children]: Raw): Entry<Pages> => ({
    no,
    title,
    amount: pages as Pages,
    children: children.map(toPages),
  });
  for (const [trees, r, d, t] of cases) {
    const got = progress(trees.map(toPages), r);
    check(got.done === d && got.total === t, `functional ${d}/${t}`);
  }
  check(percent(progress(book.map(toPages), read)) === 62, "62%");
  for (const [d, t, p] of percents) {
    const pr: Progress<Pages> = { done: d as Pages, total: t as Pages };
    check(percent(pr) === p, `percent ${d}/${t}`);
  }

  // Minutes work the same way, on their own.
  // 分も同じように使える（分どうしで）
  const talk: Entry<Minutes> = {
    no: 1, title: "講演", amount: 90 as Minutes, children: [],
  };
  const minutes = progress([talk], new Set([1]));
  check(minutes.done === 90 && minutes.total === 90, "minutes");

  // tsc stops these lines (not run).
  // これらの行は tsc が止める（動かさない）
  const typeErrors = () => {
    const pages = progress(book.map(toPages), read);
    // Pages and minutes do not add up. / ページと分は足せない
    // @ts-expect-error
    add(pages, minutes);
    // A plain number is not Pages. / ただの数は Pages ではない
    // @ts-expect-error
    const bad: Entry<Pages> = { no: 9, title: "x", amount: 3, children: [] };
    void bad;
  };
  void typeErrors;
}
