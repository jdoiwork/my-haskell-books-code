// ANCHOR: all
// The form for a new work: title, creator, kind, and the table typed as
// indented lines. The screen reads the lines (readOutline) and says
// which line is wrong before anything is sent; the shelf checks the rest
// (an empty title, a heading twice, ...) and says where.
// 新しい作品のフォーム：題・作者・種類と、字下げで書いた目次。行を読むのは
// 画面（readOutline）で、送る前に、どの行がまずいかを言う。残り（空の題、
// 2回出てくる見出し……）は本棚が確かめて、どこかを言う
import { useState } from "react";

import { shelf } from "./api";
import { kinds, unitOf } from "./kinds";
import { outlineOf, readOutline } from "./outline";

const example = `第1部 ドラゴンを知る 2
  第1章 見分け方 4
  第2章 すみかと季節 24`;

// ANCHOR: form
export function NewWork({ onAdded }: { onAdded: (id: number) => void }) {
  const [title, setTitle] = useState("");
  const [creator, setCreator] = useState("");
  const [kind, setKind] = useState("book");
  const [minutes, setMinutes] = useState(0);
  const [lines, setLines] = useState("");
  const [problems, setProblems] = useState<string[]>([]);

  const unit = unitOf(kind);
  const add = async (e: React.FormEvent) => {
    e.preventDefault();
    const read = readOutline(unit === null ? "" : lines);
    if (!read.ok) { setProblems(read.problems); return; }
    const a = await shelf.addWork({
      title, creator, kind,
      minutes: kind === "film" ? minutes : null,
      toc: outlineOf(read.rows),
    });
    if (a.ok) onAdded(a.value.work.id);
    else setProblems(a.problem.messages);
  };

  return (
    <form className="edit" onSubmit={add}>
      <h1>本を足す</h1>
      <Problems messages={problems} />
      <label>題 <input value={title}
                      onChange={(e) => setTitle(e.target.value)} /></label>
      <label>作者 <input value={creator}
        onChange={(e) => setCreator(e.target.value)} /></label>
      <label>種類 <select value={kind}
                         onChange={(e) => setKind(e.target.value)}>
        {kinds.map((k) =>
          <option key={k.word} value={k.word}>{k.label}</option>)}
      </select></label>
      {kind === "film" && <label>分 <input type="number" value={minutes}
          onChange={(e) => setMinutes(Number(e.target.value))} /></label>}
      {unit !== null && <label className="outline">
        目次（1段は空白2つ。行の最後の数は{unit}）
        <textarea rows={10} value={lines} placeholder={example}
                  onChange={(e) => setLines(e.target.value)} />
      </label>}
      <button className="primary">本棚に足す</button>
    </form>
  );
}
// ANCHOR_END: form

// ANCHOR: problems
// ⚠️ Why it was refused: every message, as the shelf (or the screen)
//    said it. The messages already say where.
// 断られた理由：本棚（か画面）が言ったとおりに、全部。どこかは、言葉が
// もう言っている
export function Problems({ messages }: { messages: string[] }) {
  if (messages.length === 0) return null;
  return (
    <div className="problem" role="alert">
      <ul>{messages.map((m) => <li key={m}>{m}</li>)}</ul>
    </div>
  );
}
// ANCHOR_END: problems
// ANCHOR_END: all
