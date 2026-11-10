// ANCHOR: all
// 🌳 The table, as nested lists: a tree drawn by a component that draws
//    its own children with itself. Each row has a box to tick: "I have
//    read this entry". Ticking or unticking only asks (onMark); what is
//    read comes back from the shelf with the card.
// 目次を入れ子のリストで。木を描くのは、自分の子を自分で描くコンポーネント。
// どの行にも「この項目を読んだ」のチェックがある。チェックは頼むだけ
// （onMark）。読んだかどうかは、カードと一緒に本棚から返ってくる
import type { TocItem } from "./tsundoku-api";

export function Toc({ items, onMark }: {
  items: TocItem[];
  onMark: (entry: number, read: boolean) => void;
}) {
  return (
    <ul className="toc">
      {items.map((item) => (
        <li key={item.id}>
          <label className={item.done ? "done" : "unread"}>
            <input type="checkbox" checked={item.done}
                   onChange={(e) => onMark(item.id, e.target.checked)} />
            {item.heading}
          </label>
          {item.children.length > 0 &&
            <Toc items={item.children} onMark={onMark} />}
        </li>
      ))}
    </ul>
  );
}
// ANCHOR_END: all
