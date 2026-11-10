// ANCHOR: all
// 🌳 The table, as nested rows: a tree drawn by a component that draws
//    its own children with itself. Each row has a box to tick: "I have
//    read this entry". Ticking or unticking only asks (onMark); what is
//    read comes back from the shelf with the card.
// 目次を入れ子の行で。木を描くのは、自分の子を自分で描くコンポーネント。
// どの行にも「この項目を読んだ」のチェックがある。チェックは頼むだけ
// （onMark）。読んだかどうかは、カードと一緒に本棚から返ってくる
import { Checkbox, Stack } from "@mantine/core";

import { tone } from "./theme";
import type { TocItem } from "./tsundoku-api";

export function Toc({ items, onMark }: {
  items: TocItem[];
  onMark: (entry: number, read: boolean) => void;
}) {
  return (
    <Stack gap={6}>
      {items.map((item) => (
        <div key={item.id}>
          <Checkbox label={item.heading} checked={item.done} color={tone.done}
                    c={item.done ? tone.done : undefined}
                    onChange={(e) =>
                      onMark(item.id, e.currentTarget.checked)} />
          {item.children.length > 0 &&
            <Stack pl="lg" pt={6}>
              <Toc items={item.children} onMark={onMark} />
            </Stack>}
        </div>
      ))}
    </Stack>
  );
}
// ANCHOR_END: all
