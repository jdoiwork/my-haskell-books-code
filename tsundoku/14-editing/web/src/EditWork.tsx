// ANCHOR: all
// The screen to fix a work: its title, its creator, and its table, one
// row at a time. Each row remembers the number it had, and sends it back
// when saved; that is how the shelf knows which line is which, even
// after it was renamed or moved.
// 作品を直す画面：題・作者と、目次を1行ずつ。どの行も、もとの番号を覚えて
// いて、保存するときに送り返す。名前が変わっても、動いても、本棚がどの行が
// どれかをわかるのは、そのため
import { useEffect, useRef, useState } from "react";

import { shelf } from "./api";
import { unitOf } from "./kinds";
import { Problems } from "./NewWork";
import {
  addAfter, editedOf, indent, type Losses, losses, moveDown, moveUp,
  outdent, remove, type Row, rowsOf,
} from "./outline";
import type { CardDetail } from "./tsundoku-api";

// ANCHOR: screen
export function EditWork({ id, onDone }: {
  id: number;
  onDone: () => void;
}) {
  const [card, setCard] = useState<CardDetail | null>(null);
  const [title, setTitle] = useState("");
  const [creator, setCreator] = useState("");
  const [rows, setRows] = useState<Row[]>([]);
  const [problems, setProblems] = useState<string[]>([]);
  // how many new rows were made, for their keys / 新しい行の数（key のため）
  const made = useRef(0);

  useEffect(() => {
    shelf.card(id).then((a) => {
      if (!a.ok) return;
      setCard(a.value);
      setTitle(a.value.work.title);
      setCreator(a.value.work.creator);
      setRows(rowsOf(a.value.toc));
    });
  }, [id]);

  if (card === null) return <p>読みこみ中…</p>;
  const unit = unitOf(card.work.kind);
  const lost = losses(card, rows);

  // ✏️ Change one row, or move rows about.
  // 1行を書き換える。行を動かす
  const write = (i: number, change: Partial<Row>) =>
    setRows(rows.map((r, j) => (j === i ? { ...r, ...change } : r)));
  const move = (how: (rs: Row[], i: number) => Row[], i: number) =>
    setRows(how(rows, i));
  const add = (i: number) => {
    made.current += 1;
    setRows(i < 0 ? [...rows, { key: `new-${made.current}`, id: null,
                                depth: 0, heading: "", size: 0 }]
                  : addAfter(rows, i, `new-${made.current}`));
  };

  // 💾 Send the whole work back. The shelf says yes (the new card), or
  //    what is wrong and where.
  // 作品の全部を送り返す。本棚は「よし」（新しいカード）か、どこがなぜ
  // まずいかを言う
  const save = async (e: React.FormEvent) => {
    e.preventDefault();
    const a = await shelf.revise(id, { title, creator, toc: editedOf(rows) });
    if (a.ok) onDone();
    else setProblems(a.problem.messages);
  };

  return (
    <form className="edit" onSubmit={save}>
      <h1>作品を直す</h1>
      <Problems messages={problems} />
      <label>題 <input value={title}
                      onChange={(e) => setTitle(e.target.value)} /></label>
      <label>作者 <input value={creator}
        onChange={(e) => setCreator(e.target.value)} /></label>
      {unit !== null && <>
        <h2>目次<small>（量は{unit}）</small></h2>
        <ol className="rows">
          {rows.map((r, i) => (
            <li key={r.key} style={{ marginLeft: `${r.depth * 1.6}rem` }}
                className={r.id === null ? "new" : ""}>
              <input className="heading" value={r.heading}
                     aria-label="見出し"
                     onChange={(e) =>
                       write(i, { heading: e.target.value })} />
              <input className="size" type="number" value={r.size}
                     aria-label={unit}
                     onChange={(e) =>
                       write(i, { size: Number(e.target.value) })} />
              <span className="tools">
                <button type="button" title="1段上へ"
                        onClick={() => move(outdent, i)}>←</button>
                <button type="button" title="1段下へ"
                        onClick={() => move(indent, i)}>→</button>
                <button type="button" title="前へ"
                        onClick={() => move(moveUp, i)}>↑</button>
                <button type="button" title="後ろへ"
                        onClick={() => move(moveDown, i)}>↓</button>
                <button type="button" title="この後ろに足す"
                        onClick={() => add(i)}>＋</button>
                <button type="button" title="下の行ごと消す"
                        onClick={() => move(remove, i)}>✕</button>
              </span>
            </li>))}
        </ol>
        <button type="button" onClick={() => add(rows.length - 1)}>
          ＋ 行を足す
        </button>
      </>}
      <Warning lost={lost} />
      <p className="actions">
        <button className="primary">保存する</button>
        <button type="button" onClick={onDone}>やめる</button>
      </p>
    </form>
  );
}
// ANCHOR_END: screen

// ANCHOR: warning
// 🧳 Before saving: what the lines that are gone carry, and where it
//    goes. Nothing is said when nothing is lost.
// 保存する前に：消える行が持っているものと、その行き先。何もなくならない
// ときは、何も言わない
function Warning({ lost }: { lost: Losses }) {
  const said = [
    lost.notes > 0 && `感想 ${lost.notes} つは、作品全体への感想に移します`,
    lost.rated > 0 && `★ ${lost.rated} つは消えます`,
    lost.read  > 0 && `読んだ印 ${lost.read} つは消えます`,
  ].filter((s) => s !== false);
  if (said.length === 0) return null;
  return (
    <div className="warning">
      <p>消える行があります。保存すると：</p>
      <ul>{said.map((s) => <li key={s}>{s}</li>)}</ul>
    </div>
  );
}
// ANCHOR_END: warning
// ANCHOR_END: all
