// ANCHOR: all
// The form for a new work: title, creator, kind, and the table, one row
// at a time, with the same rows as the screen to fix a work (TocRows).
// Every row is new, so none has a number yet; it starts with one blank
// row, and rows left blank are not sent. The shelf checks the table
// (an empty heading, a heading twice, ...) and says where.
// 新しい作品のフォーム：題・作者・種類と、目次を1行ずつ。行は、作品を直す
// 画面と同じ部品（TocRows）。どの行も新しいので、まだ番号を持たない。
// はじめに空の行を1つ置き、空のままの行は送らない。
// 目次の確かめ（空の見出し、2回出てくる見出し……）は本棚がして、どこかを
// 言う
import {
  Alert, Button, Group, List, NumberInput, SegmentedControl, Stack, Text,
  TextInput, Title,
} from "@mantine/core";
import { IconAlertCircle } from "@tabler/icons-react";
import { useState } from "react";

import { shelf } from "./api";
import { kinds, unitOf } from "./kinds";
import { addAfter, outlineOf, type Row, withoutBlank } from "./outline";
import { tone } from "./theme";
import { freshKey, TocRows } from "./TocRows";

// ANCHOR: form
export function NewWork({ onAdded }: { onAdded: (id: number) => void }) {
  const [title, setTitle] = useState("");
  const [creator, setCreator] = useState("");
  const [kind, setKind] = useState("book");
  const [minutes, setMinutes] = useState(0);
  // One blank row to start writing in (dropped if left blank).
  // 書きはじめるための空の行を1つ（空のままなら、送らない）
  const [rows, setRows] = useState<Row[]>(() => addAfter([], -1, freshKey()));
  const [problems, setProblems] = useState<string[]>([]);

  const unit = unitOf(kind);
  const add = async (e: React.FormEvent) => {
    e.preventDefault();
    const a = await shelf.addWork({
      title, creator, kind,
      minutes: kind === "film" ? minutes : null,
      toc: unit === null ? [] : outlineOf(withoutBlank(rows)),
    });
    if (a.ok) onAdded(a.value.work.id);
    else setProblems(a.problem.messages);
  };

  return (
    <form onSubmit={add}>
      <Stack>
        <Title order={2}>本を足す</Title>
        <Problems messages={problems} />
        <TextInput label="題" value={title}
                   onChange={(e) => setTitle(e.currentTarget.value)} />
        <TextInput label="作者" value={creator}
                   onChange={(e) => setCreator(e.currentTarget.value)} />
        <div>
          <Text size="sm" fw={500} mb={4}>種類</Text>
          <SegmentedControl value={kind} onChange={setKind}
            data={kinds.map((k) => ({ value: k.word, label: k.label }))} />
        </div>
        {kind === "film" &&
          <NumberInput label="分" w={120} value={minutes} min={0}
                       onChange={(v) => setMinutes(Number(v))} />}
        {unit !== null &&
          <TocRows rows={rows} setRows={setRows} unit={unit} />}
        <Group>
          <Button type="submit">本棚に足す</Button>
        </Group>
      </Stack>
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
    <Alert color={tone.danger} title="本棚に断られました" role="alert"
           icon={<IconAlertCircle />}>
      <List size="sm">
        {messages.map((m) => <List.Item key={m}>{m}</List.Item>)}
      </List>
    </Alert>
  );
}
// ANCHOR_END: problems
// ANCHOR_END: all
