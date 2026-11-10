// ANCHOR: all
// 🎛️ The form of my own attributes, drawn from the rows the API sends
//    (the same rows htmx draws, and brick shows). `kind` decides the
//    input; nothing here knows 推し度.
// 自分で決めた属性のフォーム。API が送る行から描く（htmx が描き、brick が
// 見せるのと同じ行）。入力欄を決めるのは kind。ここには推し度を知っている
// 行は1つもない
import {
  Button, NumberInput, Select, Stack, Switch, Text, TextInput,
} from "@mantine/core";
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
  // Every value goes back as words, as it came: the core reads them.
  // どの値も、来たときと同じく言葉で返す。読むのは核
  const input = (f: FormField) => {
    switch (f.kind) {
      case "number":
        return <NumberInput key={f.label} label={f.label} value={value(f)}
                            onChange={(v) => set(f, String(v))} />;
      case "date": case "text":
        return <TextInput key={f.label} label={f.label} type={f.kind}
                          value={value(f)}
                          onChange={(e) => set(f, e.currentTarget.value)} />;
      case "yesno":
        return <Switch key={f.label} label={f.label}
                       checked={value(f) === "はい"}
                       onChange={(e) =>
                         set(f, e.currentTarget.checked ? "はい" : "")} />;
      case "choice":
        return <Select key={f.label} label={f.label} clearable
                       data={f.choices} value={value(f) || null}
                       onChange={(v) => set(f, v ?? "")} />;
      default:
        return <Text key={f.label} size="sm">
                 {f.label}：{f.current || "（まだ入っていない）"}
               </Text>;
    }
  };
  // ANCHOR_END: input

  return (
    <form onSubmit={save}>
      <Stack gap="sm">
        {fields.map(input)}
        <Button type="submit" variant="light" w="fit-content">
          属性を保存
        </Button>
      </Stack>
    </form>
  );
}
// ANCHOR_END: all
