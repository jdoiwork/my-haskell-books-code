// ANCHOR: all
// The table of contents on a screen, one row at a time: a heading and a
// size on each row, and the buttons to move it. The screen to add a work
// and the screen to fix one both use it. The rows themselves live in the
// screen (rows, setRows); this only draws them and changes them.
// 画面の上の目次を、1行ずつ：どの行にも見出しと量の欄と、動かすボタン。
// 作品を足す画面も、直す画面も、これを使う。行そのものは画面が持ち
// （rows・setRows）、ここは描いて、変えるだけ
import {
  ActionIcon, Button, Group, NumberInput, Paper, Stack, Text, TextInput,
  Tooltip,
} from "@mantine/core";
import {
  type Icon, IconArrowDown, IconArrowLeft, IconArrowRight, IconArrowUp,
  IconPlus, IconX,
} from "@tabler/icons-react";
import { type KeyboardEvent, useState } from "react";

import {
  addAfter, indent, moveDown, moveUp, outdent, remove, type Row,
} from "./outline";

// 🔑 A key for a new row. The number only grows, so no two new rows
//    share a key, on any screen. (React needs keys only to differ.)
// 新しい行の key。番号は増えるだけなので、どの画面でも、新しい行どうしの
// key は重ならない（React には、ちがっていれば十分）
let made = 0;
export const freshKey = () => `new-${++made}`;

// ANCHOR: rows
export function TocRows({ rows, setRows, unit }: {
  rows: Row[];
  setRows: (rows: Row[]) => void;
  unit: string;
}) {
  // ✏️ Change one row, or move rows about.
  // 1行を書き換える。行を動かす
  const write = (i: number, change: Partial<Row>) =>
    setRows(rows.map((r, j) => (j === i ? { ...r, ...change } : r)));
  const move = (how: (rs: Row[], i: number) => Row[], i: number) =>
    setRows(how(rows, i));
  // 🆕 A new row comes after row i, and gets the cursor.
  // 新しい行は i 行目の後ろに。カーソルもそこへ
  const [focused, setFocused] = useState<string | null>(null);
  const add = (i: number) => {
    const key = freshKey();
    setRows(addAfter(rows, i, key));
    setFocused(key);
  };
  // ⏎ Enter in a row adds the next row, instead of sending the form.
  //    Not while the input method is still choosing the words.
  // 行の中の Enter は、フォームを送らずに、次の行を足す。日本語入力で
  // 変換を選んでいる間の Enter は、そのまま
  const enter = (i: number) => (e: KeyboardEvent) => {
    if (e.key !== "Enter" || e.nativeEvent.isComposing) return;
    e.preventDefault();
    add(i);
  };
  // 🔘 One small button with its tip.
  // 小さなボタン1つと、その説明
  const tool = (tip: string, Shape: Icon, act: () => void) => (
    <Tooltip label={tip}>
      <ActionIcon variant="default" aria-label={tip} onClick={act}>
        <Shape size={16} />
      </ActionIcon>
    </Tooltip>
  );
  // A new row (not on the shelf yet) is tinted.
  // 新しい行（まだ本棚にない）は色付き
  const newRow = "var(--mantine-primary-color-light)";

  return (
    <Stack gap={6}>
      <Text fw={700}>
        目次 <Text span size="sm" c="dimmed">（量は{unit}）</Text>
      </Text>
      {rows.map((r, i) => (
        <Paper key={r.key} withBorder p={6} ml={`${r.depth * 1.6}rem`}
               bg={r.id === null ? newRow : undefined}>
          <Group gap="xs" wrap="nowrap">
            <TextInput flex={1} value={r.heading} aria-label="見出し"
                       autoFocus={r.key === focused} onKeyDown={enter(i)}
                       onChange={(e) =>
                         write(i, { heading: e.currentTarget.value })} />
            <NumberInput w={90} value={r.size} aria-label={unit} min={0}
                         onKeyDown={enter(i)}
                         onChange={(v) => write(i, { size: Number(v) })} />
            <ActionIcon.Group>
              {tool("1段上へ", IconArrowLeft, () => move(outdent, i))}
              {tool("1段下へ", IconArrowRight, () => move(indent, i))}
              {tool("前へ", IconArrowUp, () => move(moveUp, i))}
              {tool("後ろへ", IconArrowDown, () => move(moveDown, i))}
              {tool("この後ろに足す", IconPlus, () => add(i))}
              {tool("下の行ごと消す", IconX, () => move(remove, i))}
            </ActionIcon.Group>
          </Group>
        </Paper>))}
      <Button variant="light" leftSection={<IconPlus size={16} />}
              w="fit-content" onClick={() => add(rows.length - 1)}>
        行を足す
      </Button>
    </Stack>
  );
}
// ANCHOR_END: rows
// ANCHOR_END: all
