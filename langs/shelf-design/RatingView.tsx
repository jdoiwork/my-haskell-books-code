// The React view of the chapter shelf-design (type-checked only:
// it runs in a browser). The screen chapters draw it for real.
// shelf-design の章の React のビュー（ブラウザで動くので、型だけを
// 確かめる）。画面に描くのは、画面を取り替える章で
type Score = number & { readonly __brand: "Score" };
type RatingStyle =
  | { kind: "hundred" }
  | { kind: "stars" }
  | { kind: "outOfTen"; decimals: boolean };

// 🖼️ The shadow of a score, the same rules as in the chapter.
// 点の影。章と同じ決まり
export function render(style: RatingStyle, score: Score): string {
  const step = (s: RatingStyle) =>
    s.kind === "stars" || (s.kind === "outOfTen" && !s.decimals) ? 10 : 1;
  const steps = Math.floor((score + step(style) / 2) / step(style));
  switch (style.kind) {
    case "hundred":
      return `${score}点`;
    case "stars":
      return `★${steps / 2}`;
    case "outOfTen":
      return style.decimals ? `${(score / 10).toFixed(1)}/10` : `${steps}/10`;
  }
}

export function makeScore(n: number): Score | undefined {
  return Number.isInteger(n) && n >= 0 && n <= 100 ? (n as Score) : undefined;
}

// ANCHOR: component
// React (a component that shows a shadow; type-checked only)
// React（影を見せるコンポーネント。型だけを確かめる）
type Props = { score: Score; style: RatingStyle };

// 🖼️ The screen only borrows render; the score stays the model.
// 画面は render を借りるだけ。点はモデルのまま
export function RatingView({ score, style }: Props) {
  return <span className="rating">{render(style, score)}</span>;
}
// ANCHOR_END: component

// @ts-expect-error: a plain number is not a Score
// ただの数は Score ではない
export const wrong = <RatingView score={73} style={{ kind: "hundred" }} />;
