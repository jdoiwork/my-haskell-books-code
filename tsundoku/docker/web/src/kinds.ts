// ANCHOR: all
// The kinds of works, in the words of the API and of the screen, and
// the unit each one counts in. A film and a game have no table.
// 作品の種類を、API の言葉と画面の言葉で。それぞれ何で数えるかも。
// 映画とゲームには目次がない
export const kinds = [
  { word: "book",   label: "本",             unit: "ページ" },
  { word: "serial", label: "連載",           unit: "話" },
  { word: "series", label: "ドラマ・アニメ", unit: "分" },
  { word: "film",   label: "映画",           unit: null },
  { word: "game",   label: "ゲーム",         unit: null },
] as const;

export const unitOf = (word: string): string | null =>
  kinds.find((k) => k.word === word)?.unit ?? null;
// ANCHOR_END: all
