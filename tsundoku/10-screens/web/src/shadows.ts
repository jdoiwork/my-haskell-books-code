// ANCHOR: all
// What only this screen does with the model's numbers: its shadows.
// The words and the rounding follow the Haskell side (Tsundoku.Labels,
// Tsundoku.Rating's render), so every screen says the same thing.
// この画面だけが、モデルの数にすること：その影。言葉と丸め方は、Haskell の
// 側（Tsundoku.Labels、Tsundoku.Rating の render）に合わせてある。
// どの画面も、同じことを言うように

// ⭐ Points as stars, in steps of half a star. The middle rounds up
//    (73 → ★3.5, 75 → ★4), as Rating.render FiveStars does.
// 点を★に。半分刻み。真ん中は切り上げ（73 → ★3.5、75 → ★4）。
// Rating.render FiveStars と同じ
export function stars(points: number | null): string {
  if (points === null) return "—";
  const halves = Math.floor((points + 5) / 10);
  return `★${halves / 2}`;
}

// 🔘 The points of each button of stars: 0, 10, ..., 100.
// ★のボタンごとの点：0、10、…、100
export const marks: number[] = Array.from({ length: 11 }, (_, i) => i * 10);

// 🏷️ The words of the reading states and the events.
// 読書の状態と、できごとの言葉
const stateWords: Record<string, string> = {
  Piled: "積読", Reading: "読書中", Finished: "読了",
  Paused: "中断", Rereading: "再読",
};
const eventWords: Record<string, string> = {
  Start: "読み始める", Finish: "読み終えた", Stop: "中断する",
  Resume: "再開する", Reread: "読み返す",
};
export const stateLabel = (name: string) => stateWords[name] ?? name;
export const eventLabel = (name: string) => eventWords[name] ?? name;
// ANCHOR_END: all
