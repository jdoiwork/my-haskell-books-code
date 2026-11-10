// ANCHOR: all
// One card: the bar, the state and its buttons, the stars, the table,
// the notes, and my own attributes. After each change, read the card
// again from the API: the screen shows what is on the shelf.
// カード1枚：バー、状態とボタン、★、目次、感想、自分で決めた属性。
// 変えるたびに、API からカードを読み直す。画面は、本棚にあるとおりを見せる
import { useCallback, useEffect, useState } from "react";

import { type Answer, shelf } from "./api";
import { AttrForm } from "./AttrForm";
import { Bar } from "./Bar";
import { eventLabel, marks, stars, stateLabel, whereOf } from "./shadows";
import { Toc } from "./Toc";
import type { CardDetail, FormField } from "./tsundoku-api";

// ANCHOR: card
export function CardView({ id }: { id: number }) {
  const [card, setCard] = useState<CardDetail | null>(null);
  const [fields, setFields] = useState<FormField[]>([]);
  const [problem, setProblem] = useState<string[]>([]);

  const reload = useCallback(async () => {
    const [c, f] = await Promise.all([shelf.card(id), shelf.form(id)]);
    if (c.ok) setCard(c.value);
    if (f.ok) setFields(f.value);
  }, [id]);
  useEffect(() => { reload(); }, [reload]);

  // ✅ Change through the API, then read the card again (or say why not).
  // API を通して変えて、カードを読み直す（だめなら理由を見せる）
  const change = async (asked: Promise<Answer<unknown>>) => {
    const a = await asked;
    setProblem(a.ok ? [] : a.problem.messages);
    await reload();
  };

  if (card === null) return <p>読みこみ中…</p>;
  const now = card.work.rating === null
    ? null : Math.floor((card.work.rating + 5) / 10) * 10;
  return (
    <article>
      <h1>{card.work.title}</h1>
      <p className="creator">{card.work.creator}</p>
      {problem.length > 0 && <ul className="problem">
        {problem.map((m) => <li key={m}>{m}</li>)}</ul>}
      {card.work.progress && <Bar p={card.work.progress} />}
      <p className="reading">
        <span className="state">{stateLabel(card.work.reading)}</span>
        {card.next.map((e) => (
          <button key={e} onClick={() => change(shelf.happen(id, e))}>
            {eventLabel(e)}
          </button>))}
      </p>
      <p className="rating">
        {marks.map((m) => (
          <button key={m} className={m === now ? "now" : ""}
                  onClick={() => change(shelf.rate(id, m))}>
            {stars(m)}
          </button>))}
      </p>
      {card.toc.length > 0 && <><h2>目次</h2><Toc items={card.toc} /></>}
      <h2>感想</h2>
      <NoteForm onWrite={(body, spoiler) =>
        change(shelf.addNote(id, body, spoiler))} />
      <ul className="notes">
        {[...card.notes].reverse().map((n, i) => (
          <li key={i}>
            <span className="where">{whereOf(card.toc, n.entry)}</span>
            <time>{n.writtenAt.slice(0, 10)}</time>
            {n.spoiler
              ? <details><summary>ネタバレ</summary><p>{n.body}</p></details>
              : <p>{n.body}</p>}
          </li>))}
      </ul>
      <h2>自分で決めた属性と状態</h2>
      <AttrForm fields={fields}
                onSave={(edits) => change(shelf.editAttrs(id, edits))} />
    </article>
  );
}
// ANCHOR_END: card

// ✍️ The form for a new note.
// 新しい感想のフォーム
function NoteForm({ onWrite }: {
  onWrite: (body: string, spoiler: boolean) => Promise<void>;
}) {
  const [body, setBody] = useState("");
  const [spoiler, setSpoiler] = useState(false);
  const write = async (e: React.FormEvent) => {
    e.preventDefault();
    await onWrite(body, spoiler);
    setBody("");
    setSpoiler(false);
  };
  return (
    <form className="note-form" onSubmit={write}>
      <textarea rows={3} value={body} onChange={(e) => setBody(e.target.value)} />
      <label>
        <input type="checkbox" checked={spoiler}
               onChange={(e) => setSpoiler(e.target.checked)} />
        ネタバレ
      </label>
      <button>書く</button>
    </form>
  );
}
// ANCHOR_END: all
