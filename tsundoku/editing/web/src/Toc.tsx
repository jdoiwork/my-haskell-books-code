// ANCHOR: all
// 🌳 The table, as nested lists: a tree drawn by a component that draws
//    its own children with itself.
// 目次を入れ子のリストで。木を描くのは、自分の子を自分で描くコンポーネント
import type { TocItem } from "./tsundoku-api";

export function Toc({ items }: { items: TocItem[] }) {
  return (
    <ul className="toc">
      {items.map((item) => (
        <li key={item.id}>
          <span className={item.done ? "done" : "unread"}>
            {item.done ? "✓ " : "・"}
          </span>
          {item.heading}
          {item.children.length > 0 && <Toc items={item.children} />}
        </li>
      ))}
    </ul>
  );
}
// ANCHOR_END: all
