// The rows of a table: reading them, making trees, moving them.
// 目次の行：読む、木にする、動かす
import { describe, expect, it } from "vitest";
import {
  addAfter, editedOf, indent, losses, moveDown, moveUp, outdent,
  outlineOf, readOutline, remove, type Row, rowsOf,
} from "./outline";
import type { CardDetail, TocItem } from "./tsundoku-api";

// 🌲 Rows written short: [id, depth, heading].
// 行を短く書く：[番号, 深さ, 見出し]
const rows = (...rs: [number | null, number, string][]): Row[] =>
  rs.map(([id, depth, heading], i) =>
    ({ key: `k${i}`, id, depth, heading, size: 1 }));
const shape = (rs: Row[]) => rs.map((r) => `${r.depth}:${r.heading}`);

describe("readOutline", () => {
  // 「字下げで深さ、行の最後の数で量」
  it("reads depths from indents, and sizes from the end", () => {
    const read = readOutline("第1部 2\n  第1章 見分け方 4\n  第2章\n");
    expect(read.ok && read.rows.map((r) => [r.depth, r.heading, r.size]))
      .toEqual([[0, "第1部", 2], [1, "第1章 見分け方", 4], [1, "第2章", 0]]);
  });
  // 「全角の空白は2つ分。空の行は読み飛ばす」
  it("counts a full-width space as two, and skips blank lines", () => {
    const read = readOutline("第1部\n\n　第1章 3");
    expect(read.ok && shape(read.rows)).toEqual(["0:第1部", "1:第1章"]);
  });
  // 「読めない行は全部、何行目かつきで」
  it("says every line it cannot read, with its number", () => {
    expect(readOutline("    深すぎ\n第1部\n   半端 3")).toEqual({
      ok: false,
      problems: [
        "1行目「深すぎ」：上の行より、2段以上深くなっています",
        "3行目「半端」：字下げは、空白2つずつにしてください",
      ],
    });
  });
});

describe("trees", () => {
  const toc: TocItem[] = [
    { id: 1, heading: "第1部", size: 2, done: true, rating: null,
      children: [{ id: 2, heading: "第1章", size: 4, done: false,
                   rating: 80, children: [] }] },
    { id: 3, heading: "第2部", size: 2, done: false, rating: null,
      children: [] },
  ];
  // 「カードの目次 → 行 → 木で、もとの形に戻る」
  it("goes from the card's table to rows and back", () => {
    expect(editedOf(rowsOf(toc))).toEqual([
      { id: 1, heading: "第1部", size: 2,
        children: [{ id: 2, heading: "第1章", size: 4, children: [] }] },
      { id: 3, heading: "第2部", size: 2, children: [] },
    ]);
  });
  // 「新しい作品の下書きには、番号がない」
  it("makes an outline with no numbers for a new work", () => {
    const read = readOutline("あ 1\n  い 2");
    expect(read.ok && outlineOf(read.rows)).toEqual([
      { heading: "あ", size: 1,
        children: [{ heading: "い", size: 2, children: [] }] },
    ]);
  });
  // 「消える行の感想・★・読んだ印を数える」
  it("counts what the lines that are gone carry", () => {
    const card = {
      toc,
      notes: [{ entry: 2, body: "", writtenAt: "", spoiler: false },
              { entry: null, body: "", writtenAt: "", spoiler: false }],
    } as unknown as CardDetail;
    const kept = rowsOf(toc).filter((r) => r.id !== 1 && r.id !== 2);
    expect(losses(card, kept)).toEqual({ notes: 1, rated: 1, read: 1 });
    expect(losses(card, rowsOf(toc))).toEqual({ notes: 0, rated: 0, read: 0 });
  });
});

// ANCHOR: moving
describe("moving rows", () => {
  const book = rows([1, 0, "A"], [2, 1, "A1"], [3, 1, "A2"], [4, 0, "B"]);
  // 「→ は兄の下へ、部分木ごと。兄がいなければ動かない」
  it("indents under the older sibling, with the subtree", () => {
    expect(shape(indent(book, 3))).toEqual(["0:A", "1:A1", "1:A2", "1:B"]);
    expect(shape(indent(book, 2))).toEqual(["0:A", "1:A1", "2:A2", "0:B"]);
    expect(indent(book, 1)).toBe(book);
  });
  // 「← は親の外へ。弟たちは親に残る」
  it("outdents out of the parent, after the younger siblings", () => {
    expect(shape(outdent(book, 1))).toEqual(["0:A", "1:A2", "0:A1", "0:B"]);
    expect(outdent(book, 0)).toBe(book);
  });
  // 「↑ ↓ は兄弟と、部分木ごと入れ替える」
  it("swaps with a sibling, subtrees and all", () => {
    expect(shape(moveUp(book, 3))).toEqual(["0:B", "0:A", "1:A1", "1:A2"]);
    expect(shape(moveDown(book, 0))).toEqual(["0:B", "0:A", "1:A1", "1:A2"]);
    expect(shape(moveDown(book, 2))).toEqual(shape(book));
  });
  // 「✕ は部分木ごと消し、＋ は部分木の後ろに同じ段で足す」
  it("removes a subtree, and adds after one", () => {
    expect(shape(remove(book, 0))).toEqual(["0:B"]);
    const added = addAfter(book, 0, "new");
    expect(shape(added)).toEqual(["0:A", "1:A1", "1:A2", "0:", "0:B"]);
    expect(added[3].id).toBe(null);
  });
});
// ANCHOR_END: moving
