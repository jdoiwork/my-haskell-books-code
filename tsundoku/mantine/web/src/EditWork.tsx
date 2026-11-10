// ANCHOR: all
// The screen to fix a work: its title, its creator, and its table, one
// row at a time. Each row remembers the number it had, and sends it back
// when saved; that is how the shelf knows which line is which, even
// after it was renamed or moved.
// 作品を直す画面：題・作者と、目次を1行ずつ。どの行も、もとの番号を覚えて
// いて、保存するときに送り返す。名前が変わっても、動いても、本棚がどの行が
// どれかをわかるのは、そのため
// ANCHOR: imports
import {
  Alert, Button, Group, List, Radio, Stack, Text, TextInput, Title,
} from "@mantine/core";
import { IconAlertTriangle } from "@tabler/icons-react";
import { useEffect, useState } from "react";

import { shelf } from "./api";
import { unitOf } from "./kinds";
import { Problems } from "./NewWork";
import { editedOf, type Losses, losses, type Row, rowsOf } from "./outline";
import { tone } from "./theme";
import { TocRows } from "./TocRows";
import type { CardDetail } from "./tsundoku-api";
// ANCHOR_END: imports

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
  // what to do with the notes on lines that are gone: the safe way first
  // 消える行の感想をどうするか：はじめは安全な方
  const [removedNotes, setRemovedNotes] = useState<NotesChoice>("move");

  useEffect(() => {
    shelf.card(id).then((a) => {
      if (!a.ok) return;
      setCard(a.value);
      setTitle(a.value.work.title);
      setCreator(a.value.work.creator);
      setRows(rowsOf(a.value.toc));
    });
  }, [id]);

  if (card === null) return <Text>読みこみ中…</Text>;
  const unit = unitOf(card.work.kind);
  const lost = losses(card, rows);

  // 💾 Send the whole work back. The shelf says yes (the new card), or
  //    what is wrong and where.
  // 作品の全部を送り返す。本棚は「よし」（新しいカード）か、どこがなぜ
  // まずいかを言う
  const save = async (e: React.FormEvent) => {
    e.preventDefault();
    const a = await shelf.revise(id, {
      title, creator, toc: editedOf(rows), removedNotes,
    });
    if (a.ok) onDone();
    else setProblems(a.problem.messages);
  };

  return (
    <form onSubmit={save}>
      <Stack>
        <Title order={2}>作品を直す</Title>
        <Problems messages={problems} />
        <TextInput label="題" value={title}
                   onChange={(e) => setTitle(e.currentTarget.value)} />
        <TextInput label="作者" value={creator}
                   onChange={(e) => setCreator(e.currentTarget.value)} />
        {unit !== null &&
          <TocRows rows={rows} setRows={setRows} unit={unit} />}
        <Warning lost={lost} choice={removedNotes}
                 choose={setRemovedNotes} />
        <Group>
          <Button type="submit">保存する</Button>
          <Button variant="default" onClick={onDone}>やめる</Button>
        </Group>
      </Stack>
    </form>
  );
}
// ANCHOR_END: screen

// ANCHOR: warning
// 🧳 Before saving: what the lines that are gone carry. A rating or a
//    read mark goes with its line. A note is my own words, so I choose:
//    move it to the whole work (the safe way), or drop it with the line.
//    Nothing is said when nothing is lost.
// 保存する前に：消える行が持っているもの。★と読んだ印は、行と一緒に
// 消える。感想は自分の言葉なので、選ぶ：作品全体へ移す（安全な方）か、
// 行と一緒に消すか。何もなくならないときは、何も言わない
type NotesChoice = "move" | "drop";

function Warning({ lost, choice, choose }: {
  lost: Losses;
  choice: NotesChoice;
  choose: (c: NotesChoice) => void;
}) {
  const said = [
    lost.rated > 0 && `★ ${lost.rated} つは消えます`,
    lost.read  > 0 && `読んだ印 ${lost.read} つは消えます`,
  ].filter((s) => s !== false);
  if (lost.notes === 0 && said.length === 0) return null;
  return (
    <Alert color={tone.warn} icon={<IconAlertTriangle />}
           title="消える行があります。保存すると：">
      {said.length > 0 &&
        <List size="sm">
          {said.map((s) => <List.Item key={s}>{s}</List.Item>)}
        </List>}
      {lost.notes > 0 && (
        <Radio.Group mt="sm" value={choice}
                     onChange={(v) => choose(v as NotesChoice)}
                     label={`消える行に感想が ${lost.notes} 件あります`}>
          <Stack gap={6} mt={6}>
            <Radio value="move" label="作品全体への感想に移す" />
            <Radio value="drop" label="行と一緒に消す（もとに戻せません）" />
          </Stack>
        </Radio.Group>
      )}
    </Alert>
  );
}
// ANCHOR_END: warning
// ANCHOR_END: all
