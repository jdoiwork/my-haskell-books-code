// ANCHOR: all
// 📊 The bar of the progress: the numbers the core counted, drawn by the
//    browser. The same numbers the terminal draws with ▓.
// 進み具合のバー：核が数えた数を、ブラウザーが描く。ターミナルが ▓ で
// 描くのと同じ数
import type { ProgressNumbers } from "./tsundoku-api";

export function Bar({ p }: { p: ProgressNumbers }) {
  return (
    <span className="progress">
      <progress value={p.done} max={p.total} /> {p.percent}%
    </span>
  );
}
// ANCHOR_END: all
