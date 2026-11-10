// ANCHOR: all
// What only this screen does with the model's numbers: its shadows.
// The words and the rounding follow the Haskell side (Tsundoku.Labels,
// Tsundoku.Rating's render), so every screen says the same thing.
// この画面だけが、モデルの数にすること：その影。言葉と丸め方は、Haskell の
// 側（Tsundoku.Labels、Tsundoku.Rating の render）に合わせてある。
// どの画面も、同じことを言うように
import type { TocItem } from "./tsundoku-api";

// ⭐ Points as stars, in steps of half a star. The middle rounds up
//    (73 → ★3.5, 75 → ★4), as Rating.render FiveStars does.
// 点を★に。半分刻み。真ん中は切り上げ（73 → ★3.5、75 → ★4）。
// Rating.render FiveStars と同じ
export function stars(points: number | null): string {
  if (points === null) return "—";
  return `★${halfStars(points)}`;
}

// ANCHOR: halfStars
// ⭐ The same stars as a number, for Mantine's Rating (fractions={2}):
//    73 → 3.5. And back: a star is 20 points, so 3.5 → 70.
// 同じ★を数で。Mantine の Rating（fractions={2}）のために：73 → 3.5。
// そして、もとへ：★1つは 20 点なので、3.5 → 70
export const halfStars = (points: number): number =>
  Math.floor((points + 5) / 10) / 2;

export const pointsOf = (starsGiven: number): number =>
  Math.round(starsGiven * 20);
// ANCHOR_END: halfStars

// 🔘 The points of each button of stars: 0, 10, ..., 100.
// ★のボタンごとの点：0、10、…、100
export const marks: number[] = Array.from({ length: 11 }, (_, i) => i * 10);

// 🏷️ The words of the reading states and the events.
// 読書の状態と、できごとの言葉
const stateWords: Record<string, string> = {
  Piled: "積読", Reading: "読書中", Finished: "読了",
  Paused: "中断",
};
const eventWords: Record<string, string> = {
  Start: "読み始める", Finish: "読み終えた", Stop: "中断する",
  Resume: "再開する", Reread: "読み返す",
};
export const stateLabel = (name: string) => stateWords[name] ?? name;
export const eventLabel = (name: string) => eventWords[name] ?? name;

// ANCHOR: stateColor
// 🎨 The colour of each state's badge (Mantine's colour names). Only
//    this screen colours them; the API only said the name.
// 状態の札の色（Mantine の色の名前）。色を付けるのは、この画面だけ。
// API は名前を言っただけ
const stateColors: Record<string, string> = {
  Piled: "gray", Reading: "indigo", Finished: "teal", Paused: "orange",
};
export const stateColor = (name: string) => stateColors[name] ?? "gray";
// ANCHOR_END: stateColor

// 📍 Which entry a note is about, as Html.Parts' notePart says it: the
//    heading, the whole work, or an entry the table no longer has.
// 感想が、どの項目へのものか。Html.Parts の notePart と同じ言い方で：
// 見出し、作品の全体、目次にもうない項目
export function whereOf(toc: TocItem[], entry: number | null): string {
  if (entry === null) return "作品の全体";
  const found = flatItems(toc).find((item) => item.id === entry);
  return found ? found.heading : "（目次にない項目）";
}

// 🌳 Every item of the table, from the top.
// 目次の項目の全部を、上から順に
function flatItems(items: TocItem[]): TocItem[] {
  return items.flatMap((item) => [item, ...flatItems(item.children)]);
}
// ANCHOR_END: all
