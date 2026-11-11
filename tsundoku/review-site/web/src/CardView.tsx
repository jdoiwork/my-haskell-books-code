// ANCHOR: all
// One card: the bar, the state and its buttons, the stars, the table,
// the notes, and my own attributes. After each change, read the card
// again from the API: the screen shows what is on the shelf.
// カード1枚：バー、状態とボタン、★、目次、感想、自分で決めた属性。
// 変えるたびに、API からカードを読み直す。画面は、本棚にあるとおりを見せる
import { useCallback, useEffect, useRef, useState } from "react";

import { type Answer, shelf } from "./api";
import { AttrForm } from "./AttrForm";
import { Bar } from "./Bar";
import { dayOf } from "./day";
import { marksFor } from "./outline";
import { eventLabel, marks, stars, stateLabel, whereOf } from "./shadows";
import { Toc } from "./Toc";
import type { CardDetail, FormField } from "./tsundoku-api";

// ANCHOR: card
// ANCHOR: props
export function CardView({ id, onEdit }: {
  id: number;
  onEdit: () => void;
}) {
// ANCHOR_END: props
  const [card, setCard] = useState<CardDetail | null>(null);
  const [fields, setFields] = useState<FormField[]>([]);
  const [problem, setProblem] = useState<string[]>([]);

  // 🔁 Read the card again. Two readings can overlap (two quick clicks),
  //    and their answers can come back in either order: show only the
  //    newest one asked, and drop an older answer that comes late.
  // カードを読み直す。読み直しは重なることがあり（すばやく2回押す）、
  // 答えはどちらが先に届くかわからない。いちばん新しく頼んだものだけを
  // 見せ、遅れて届いた古い答えは捨てる
  const asked = useRef(0);
  const reload = useCallback(async () => {
    const mine = ++asked.current;
    const [c, f] = await Promise.all([shelf.card(id), shelf.form(id)]);
    if (mine !== asked.current) return;
    if (c.ok) setCard(c.value);
    else setProblem(c.problem.messages);
    if (f.ok) setFields(f.value);
    else setProblem(f.problem.messages);
  }, [id]);
  useEffect(() => { reload(); }, [reload]);

  // ✅ Change through the API, then read the card again (or say why not).
  //    Says whether it worked, so a form keeps what was typed on a no.
  // API を通して変えて、カードを読み直す（だめなら理由を見せる）。
  // うまくいったかを返す（だめなら、フォームは打ったものを残す）
  const change = async (sent: Promise<Answer<unknown>>) => {
    const a = await sent;
    setProblem(a.ok ? [] : a.problem.messages);
    await reload();
    return a.ok;
  };

  // ANCHOR: mark
  // ✅ A tick writes today in the log, for the entry and every entry
  //    under it; taking it off drops their lines. One at a time, and
  //    the first refusal stops the rest.
  // チェックを入れたら、その項目と下の全部について、今日の日付でログに
  // 書く。外したら、その行を消す。1つずつ順に、断られたらそこで止める
  const mark = (entry: number, read: boolean) => {
    if (card === null) return;
    const today = dayOf(new Date());
    const asks = marksFor(card.toc, entry, read).map((e) => () =>
      read ? shelf.logReading(id, today, e) : shelf.unread(id, e));
    change(inOrder(asks));
  };
  // ANCHOR_END: mark

  if (card === null) return problem.length > 0
    ? <ul className="problem">{problem.map((m) => <li key={m}>{m}</li>)}</ul>
    : <p>読みこみ中…</p>;
  const now = card.work.rating === null
    ? null : Math.floor((card.work.rating + 5) / 10) * 10;
  return (
    <article>
      {/* ANCHOR: fix */}
      <h1>{card.work.title}</h1>
      <p className="creator">{card.work.creator}
        <button className="fix" onClick={onEdit}>✏️ 直す</button></p>
      {/* ANCHOR_END: fix */}
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
      {/* ANCHOR: toc */}
      {card.toc.length > 0 && <>
        <h2>目次</h2>
        <Toc items={card.toc} onMark={mark} />
      </>}
      {/* ANCHOR_END: toc */}
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

// ANCHOR: inOrder
// 🔁 Ask one after another; the first refusal stops the rest.
// 1つずつ順に聞く。断られたら、そこで止める
async function inOrder(
  asks: (() => Promise<Answer<unknown>>)[],
): Promise<Answer<unknown>> {
  let last: Answer<unknown> = { ok: true, value: null };
  for (const ask of asks) {
    last = await ask();
    if (!last.ok) break;
  }
  return last;
}
// ANCHOR_END: inOrder

// ✍️ The form for a new note.
// 新しい感想のフォーム
function NoteForm({ onWrite }: {
  onWrite: (body: string, spoiler: boolean) => Promise<boolean>;
}) {
  const [body, setBody] = useState("");
  const [spoiler, setSpoiler] = useState(false);
  // Clear the form only when the note was written.
  // 書けたときだけ、フォームを空にする
  const write = async (e: React.FormEvent) => {
    e.preventDefault();
    if (await onWrite(body, spoiler)) {
      setBody("");
      setSpoiler(false);
    }
  };
  return (
    <form className="note-form" onSubmit={write}>
      <textarea rows={3} value={body}
                onChange={(e) => setBody(e.target.value)} />
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
