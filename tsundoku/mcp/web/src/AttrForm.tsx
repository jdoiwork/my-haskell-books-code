// ANCHOR: all
// 🎛️ The form of my own attributes, drawn from the rows the API sends
//    (the same rows htmx draws, and brick shows). `kind` decides the
//    input; nothing here knows 推し度.
// 自分で決めた属性のフォーム。API が送る行から描く（htmx が描き、brick が
// 見せるのと同じ行）。入力欄を決めるのは kind。ここには推し度を知っている
// 行は1つもない
import { useState } from "react";

import type { FormField } from "./tsundoku-api";

export function AttrForm({ fields, onSave }: {
  fields: FormField[];
  onSave: (edits: Record<string, string>) => Promise<boolean>;
}) {
  // What I typed, over the values that came. They go away only when
  // saved: a no, or the card read again after another change, keeps them.
  // 来た値の上に、打ったもの。消えるのは保存できたときだけ。断られても、
  // ほかの変更でカードを読み直しても、残る
  const [edits, setEdits] = useState<Record<string, string>>({});
  const value = (f: FormField) => edits[f.label] ?? f.current;
  const set = (f: FormField, v: string) =>
    setEdits((now) => ({ ...now, [f.label]: v }));
  const save = async (e: React.FormEvent) => {
    e.preventDefault();
    if (await onSave(edits)) setEdits({});
  };

  // ANCHOR: input
  const input = (f: FormField) => {
    switch (f.kind) {
      case "number": case "date": case "text":
        return <input type={f.kind} value={value(f)}
                      onChange={(e) => set(f, e.target.value)} />;
      case "yesno":
        return <input type="checkbox" checked={value(f) === "はい"}
                      onChange={(e) =>
                        set(f, e.target.checked ? "はい" : "")} />;
      case "choice":
        return (
          <select value={value(f)} onChange={(e) => set(f, e.target.value)}>
            <option value="">—</option>
            {f.choices.map((c) => <option key={c} value={c}>{c}</option>)}
          </select>);
      default:
        return <span>{f.current || "（まだ入っていない）"}</span>;
    }
  };
  // ANCHOR_END: input

  return (
    <form onSubmit={save}>
      {fields.map((f) => (
        <label key={f.label} className="attr">
          <span>{f.label}</span>{input(f)}
        </label>))}
      <button>属性を保存</button>
    </form>
  );
}
// ANCHOR_END: all
