// A note shown or hidden, as the API sends it: the "tag" says which.
// 見せる感想か、隠した感想か。API が送るとおりに。どちらかは "tag" が言う
type NoteItem = { entry: number | null; body: string };
type SeenNote =
  | { tag: "ShownNote"; note: NoteItem }
  | { tag: "HiddenNote"; entry: number | null; hint: string };

const seen: SeenNote[] = [
  { tag: "ShownNote", note: { entry: null, body: "絵が多くて読みやすい" } },
  { tag: "HiddenNote", entry: 8, hint: "ネタバレ（「第4章」を読んだら見えます）" },
];

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function linesOf(notes: SeenNote[]): string[] {
  const lines: string[] = [];
  for (const s of notes) {
    if (s.tag === "ShownNote") {
      lines.push(s.note.body);          // here s has note
    } else {
      lines.push(s.hint);               // here s has hint, and no body
    }
  }
  return lines;
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style)
// TypeScript（関数型スタイル）
const lineOf = (s: SeenNote): string => {
  switch (s.tag) {
    case "ShownNote":  return s.note.body;
    case "HiddenNote": return s.hint;
    default: {
      // a third kind would not fit in `never`: tsc stops here
      // 3つ目の種類が増えたら never に入らないので、tsc がここで止まる
      const left: never = s;
      return left;
    }
  }
};
const lines = seen.map(lineOf);
// ANCHOR_END: functional

const expected = ["絵が多くて読みやすい", "ネタバレ（「第4章」を読んだら見えます）"];
if (JSON.stringify(linesOf(seen)) !== JSON.stringify(expected)) throw new Error("procedural");
if (JSON.stringify(lines) !== JSON.stringify(expected)) throw new Error("functional");
console.log("ok");
