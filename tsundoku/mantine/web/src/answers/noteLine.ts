// ANCHOR: all
// Training: one line for a note, shown or hidden, with no way to forget
// a kind. If the API grows a third kind of SeenNote (after gen-ts),
// `never` makes tsc stop here.
// トレーニング：感想を1行に。見せるものも隠したものも。種類を忘れる道は
// ない。API の SeenNote に3つ目の種類が増えたら（gen-ts のあと）、never が
// tsc をここで止める
import type { SeenNote } from "../tsundoku-api";

export function noteLine(s: SeenNote): string {
  switch (s.tag) {
    case "ShownNote":
      return s.note.spoiler ? `（ネタバレ）${s.note.body}` : s.note.body;
    case "HiddenNote":
      return `🔒 ${s.hint}`;
    default: {
      const left: never = s;
      return left;
    }
  }
}
// ANCHOR_END: all
