// ANCHOR: all
// The table of contents as rows on a screen: a flat list, each row with
// its depth. Easy to draw and to change one row at a time; turned into a
// tree (EditedItem, Outline) only when it is sent. No React here, so
// every function is tested by itself (outline.test.ts).
// 画面の上の目次は、行の平らなリスト。どの行も深さを持つ。描くのも1行ずつ
// 変えるのも簡単で、木（EditedItem・Outline）にするのは送るときだけ。
// ここに React はないので、関数を1つずつテストできる（outline.test.ts）
import type {
  CardDetail, EditedItem, Outline, TocItem,
} from "./tsundoku-api";

// ANCHOR: row
// 📄 One row: the number it had (null if new), how deep, and what it
//    says. key is only for React, to tell rows apart while they move.
// 1行：もとの番号（新しければ null）、深さ、中身。key は React のためだけ
// のもので、行が動いても見分けられるように
export type Row = {
  key: string;
  id: number | null;
  depth: number;
  heading: string;
  size: number;
};
// ANCHOR_END: row

// ANCHOR: tree
// 🌳 Rows to a tree, and a tree to rows. A row's children are the rows
//    right below it that are deeper.
// 行を木に、木を行に。行の子は、すぐ下に続く、もっと深い行たち
type Node = { row: Row; children: Node[] };

function nest(rows: Row[]): Node[] {
  const top: Node[] = [];
  const path: Node[] = [];          // the open nodes, one per depth
  for (const row of rows) {
    const node = { row, children: [] };
    path.length = row.depth;
    (row.depth === 0 ? top : path[row.depth - 1].children).push(node);
    path.push(node);
  }
  return top;
}

export function editedOf(rows: Row[]): EditedItem[] {
  const edited = (n: Node): EditedItem => ({
    id: n.row.id, heading: n.row.heading, size: n.row.size,
    children: n.children.map(edited),
  });
  return nest(rows).map(edited);
}

export function outlineOf(rows: Row[]): Outline[] {
  const outline = (n: Node): Outline => ({
    heading: n.row.heading, size: n.row.size,
    children: n.children.map(outline),
  });
  return nest(rows).map(outline);
}

export function rowsOf(toc: TocItem[], depth = 0): Row[] {
  return toc.flatMap((t) => [
    { key: `id-${t.id}`, id: t.id, depth, heading: t.heading, size: t.size },
    ...rowsOf(t.children, depth + 1),
  ]);
}
// ANCHOR_END: tree

// ANCHOR: moves
// 🔀 Changes to the rows. A row moves with everything under it (its
//    subtree: the rows after it that are deeper), so the tree is never
//    broken. A change that cannot be made gives the rows back as they are.
// 行の変え方。行は、その下のもの全部（部分木：後ろに続く、もっと深い行）と
// 一緒に動く。だから木は壊れない。できない変え方なら、行をそのまま返す

// The end (one past the last row) of the subtree of row i.
// 行 i の部分木の終わり（最後の行の1つ後ろ）
function end(rows: Row[], i: number): number {
  let j = i + 1;
  while (j < rows.length && rows[j].depth > rows[i].depth) j++;
  return j;
}

// The row before i on the same level (its older sibling), if any.
// 行 i と同じ段の、ひとつ前の行（兄）。なければ -1
function before(rows: Row[], i: number): number {
  for (let j = i - 1; j >= 0 && rows[j].depth >= rows[i].depth; j--) {
    if (rows[j].depth === rows[i].depth) return j;
  }
  return -1;
}

// Shift row i and its subtree by d levels.
// 行 i と部分木を、d 段ずらす
function shift(rows: Row[], i: number, d: number): Row[] {
  const e = end(rows, i);
  return rows.map((r, j) =>
    (j >= i && j < e ? { ...r, depth: r.depth + d } : r));
}

// → Under the older sibling. ← Up one level, out of the parent: the
//   younger siblings stay with the parent, so the row moves after them.
// → 兄の下へ。← 1段上へ、親の外へ。弟たちは親に残るので、行は弟たちの
//   後ろへ動く
export const indent = (rows: Row[], i: number): Row[] =>
  before(rows, i) < 0 ? rows : shift(rows, i, 1);

export function outdent(rows: Row[], i: number): Row[] {
  if (rows[i].depth === 0) return rows;
  let parent = i - 1;
  while (rows[parent].depth >= rows[i].depth) parent--;
  const e = end(rows, i);
  const after = end(rows, parent);
  const moved = shift(rows, i, -1).slice(i, e);
  return [...rows.slice(0, i), ...rows.slice(e, after), ...moved,
          ...rows.slice(after)];
}

// ↑ ↓ Swap with the sibling before or after.
// ↑ ↓ 前か後ろの兄弟と入れ替える
export function moveUp(rows: Row[], i: number): Row[] {
  const b = before(rows, i);
  if (b < 0) return rows;
  const e = end(rows, i);
  return [...rows.slice(0, b), ...rows.slice(i, e), ...rows.slice(b, i),
          ...rows.slice(e)];
}

export function moveDown(rows: Row[], i: number): Row[] {
  const e = end(rows, i);
  if (e >= rows.length || rows[e].depth !== rows[i].depth) return rows;
  return moveUp(rows, e);
}

// ✕ Remove a row with its subtree. ＋ A new row after the subtree, on
//   the same level; with no rows yet (i is -1), the first row.
// ✕ 行を部分木ごと消す。＋ 部分木の後ろ、同じ段に新しい行。行がまだ
//   なければ（i が -1）、1行目
export const remove = (rows: Row[], i: number): Row[] =>
  [...rows.slice(0, i), ...rows.slice(end(rows, i))];

export function addAfter(rows: Row[], i: number, key: string): Row[] {
  if (i < 0) return [{ key, id: null, depth: 0, heading: "", size: 0 },
                     ...rows];
  const e = end(rows, i);
  const row = { key, id: null, depth: rows[i].depth, heading: "", size: 0 };
  return [...rows.slice(0, e), row, ...rows.slice(e)];
}

// 🧹 New rows left blank (no heading, no size, no rows under them) are
//    dropped before sending, so the blank row a screen starts with costs
//    nothing when the work has no table.
// 空のままの新しい行（見出しも量もなく、下にも行がない）は、送る前に落とす。
// 画面がはじめに置く空の行は、目次のない作品なら、そのままでかまわない
export function withoutBlank(rows: Row[]): Row[] {
  const blank = (r: Row, i: number) =>
    r.id === null && r.heading.trim() === "" && r.size === 0
      && !(i + 1 < rows.length && rows[i + 1].depth > r.depth);
  return rows.filter((r, i) => !blank(r, i));
}
// ANCHOR_END: moves

// ANCHOR: marks
// ✅ The entries a tick changes: the entry and every entry under it,
//    leaving out those already as asked (so an entry read before gets
//    no second line in the log).
// チェック1つで変わる項目：その項目と、その下の全部。もう望みどおりの
// 項目は外す（読んだ項目に、ログの2行目を書かないように）
export function marksFor(toc: TocItem[], entry: number,
                         read: boolean): number[] {
  const under = (item: TocItem): TocItem[] =>
    [item, ...item.children.flatMap(under)];
  const find = (items: TocItem[]): TocItem | undefined => {
    for (const item of items) {
      if (item.id === entry) return item;
      const found = find(item.children);
      if (found) return found;
    }
    return undefined;
  };
  const top = find(toc);
  return top === undefined
    ? []
    : under(top).filter((item) => item.done !== read).map((item) => item.id);
}
// ANCHOR_END: marks

// ANCHOR: losses
// 🧳 What a save would carry over or drop, from the lines that are gone:
//    notes move to the whole work; ratings and read marks are dropped.
//    (Texts of entries too, but the card does not show them.)
// 保存したら、消える行の何が移り、何が消えるか：感想は作品全体へ移り、
// ★と読んだ印は消える。（項目の本文も消えるが、カードには出ていない）
export type Losses = { notes: number; rated: number; read: number };

export function losses(card: CardDetail, rows: Row[]): Losses {
  const kept = new Set(rows.map((r) => r.id));
  const gone = (items: TocItem[]): TocItem[] => items.flatMap((t) =>
    [...(kept.has(t.id) ? [] : [t]), ...gone(t.children)]);
  const lost = gone(card.toc);
  const ids = new Set(lost.map((t) => t.id));
  return {
    notes: card.notes.filter((n) => n.entry !== null && ids.has(n.entry))
                     .length,
    rated: lost.filter((t) => t.rating !== null).length,
    read:  lost.filter((t) => t.done).length,
  };
}
// ANCHOR_END: losses
// ANCHOR_END: all
