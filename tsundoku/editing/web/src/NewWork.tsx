// ANCHOR: all
// The form for a new work: title, creator, kind, and the table, one row
// at a time, with the same rows as the screen to fix a work (TocRows).
// Every row is new, so none has a number yet. The shelf checks the table
// (an empty heading, a heading twice, ...) and says where.
// 新しい作品のフォーム：題・作者・種類と、目次を1行ずつ。行は、作品を直す
// 画面と同じ部品（TocRows）。どの行も新しいので、まだ番号を持たない。
// 目次の確かめ（空の見出し、2回出てくる見出し……）は本棚がして、どこかを
// 言う
import { useState } from "react";

import { shelf } from "./api";
import { kinds, unitOf } from "./kinds";
import { outlineOf, type Row } from "./outline";
import { TocRows } from "./TocRows";

// ANCHOR: form
export function NewWork({ onAdded }: { onAdded: (id: number) => void }) {
  const [title, setTitle] = useState("");
  const [creator, setCreator] = useState("");
  const [kind, setKind] = useState("book");
  const [minutes, setMinutes] = useState(0);
  const [rows, setRows] = useState<Row[]>([]);
  const [problems, setProblems] = useState<string[]>([]);

  const unit = unitOf(kind);
  const add = async (e: React.FormEvent) => {
    e.preventDefault();
    const a = await shelf.addWork({
      title, creator, kind,
      minutes: kind === "film" ? minutes : null,
      toc: unit === null ? [] : outlineOf(rows),
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
      {unit !== null &&
        <TocRows rows={rows} setRows={setRows} unit={unit} />}
      <p className="actions">
        <button className="primary">本棚に足す</button>
      </p>
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
