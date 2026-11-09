// ANCHOR: all
// 🎛️ The form of my own attributes, drawn from the rows the API sends
//    (the same rows htmx draws, and brick shows). `kind` decides the
//    input; nothing here knows 推し度.
// 自分で決めた属性のフォーム。API が送る行から描く（htmx が描き、brick が
// 見せるのと同じ行）。入力欄を決めるのは kind。ここには推し度を知っている
// 行は1つもない
import { useEffect, useState } from "react";

import type { FormField } from "./tsundoku-api";

export function AttrForm({ fields, onSave }: {
  fields: FormField[];
  onSave: (edits: Record<string, string>) => Promise<void>;
}) {
  const [edits, setEdits] = useState<Record<string, string>>({});
  useEffect(() => { setEdits({}); }, [fields]);
  const value = (f: FormField) => edits[f.label] ?? f.current;
  const set = (f: FormField, v: string) => setEdits({ ...edits, [f.label]: v });

  // ANCHOR: input
  const input = (f: FormField) => {
    switch (f.kind) {
      case "number": case "date": case "text":
        return <input type={f.kind} value={value(f)}
                      onChange={(e) => set(f, e.target.value)} />;
      case "yesno":
        return <input type="checkbox" checked={value(f) === "はい"}
                      onChange={(e) => set(f, e.target.checked ? "はい" : "")} />;
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
    <form onSubmit={(e) => { e.preventDefault(); onSave(edits); }}>
      {fields.map((f) => (
        <label key={f.label} className="attr">
          <span>{f.label}</span>{input(f)}
        </label>))}
      <button>属性を保存</button>
    </form>
  );
}
// ANCHOR_END: all
