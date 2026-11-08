# The progress of reading in Python, for the chapter
# "The tree of contents and how far you have read".
# 「目次の木と進み具合」の章の、読んだ量の集計を Python で

# ANCHOR: procedural
# Python (procedural style: walk the tree with a stack)
# Python（手続き型スタイル：スタックで木を歩く）

# 📖 Add up the pages read and all the pages of the contents.
#    Each entry's pages are its own, not its children's.
# 目次の、読んだ量と全体の量を足していく。
# 各項目の量は、その項目だけの量（子の量は入らない）
def progress(entries, read):
    done = 0
    total = 0
    stack = list(entries)
    while stack:
        entry = stack.pop()
        total += entry["pages"]
        if entry["no"] in read:
            done += entry["pages"]
        stack.extend(entry["children"])
    return done, total


# 📊 Percent, rounded half up; 1 to 99 while you are reading.
# 百分率。四捨五入して、読んでいる途中は 1〜99 におさめる
def percent(done, total):
    if total == 0:
        return 0
    p = (done * 200 + total) // (2 * total)
    if done > 0 and p == 0:
        p = 1          # started: not 0% / 読み始めたら 0% にしない
    if done < total and p == 100:
        p = 99         # not finished: not 100% / 読み終わるまで 100% にしない
    return p
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: recursion, a generator and sum)
# Python（関数型スタイル：再帰とジェネレーターと sum）
from collections.abc import Iterator
from dataclasses import dataclass


# 📑 One entry of the contents, with its own pages and its children.
# 目次の1項目。その項目だけの量と、子の項目を持つ
@dataclass(frozen=True)
class Entry:
    no: int
    title: str
    pages: int
    children: tuple["Entry", ...] = ()


# 🌳 Yield the entries of a tree from the top, in order (preorder).
# 木の項目を、上から順に（前順に）1つずつ出す
def walk(entry: Entry) -> Iterator[Entry]:
    yield entry
    for child in entry.children:
        yield from walk(child)


# ➕ Progress is a value you can add up, starting from Progress(0, 0).
# 進み具合は、足し算できる値。Progress(0, 0) から足し始める
@dataclass(frozen=True)
class Progress:
    done: int
    total: int

    def __add__(self, other: "Progress") -> "Progress":
        return Progress(self.done + other.done,
                        self.total + other.total)

    def percent(self) -> int:
        if self.total == 0:
            return 0
        p = (self.done * 200 + self.total) // (2 * self.total)
        if 0 < self.done < self.total:
            return min(max(p, 1), 99)
        return p


# 📖 One entry's own progress: all its pages, or none of them.
# 1項目だけの進み具合：量を全部読んだか、まったく読んでいないか
def own(entry: Entry, read: set[int]) -> Progress:
    done = entry.pages if entry.no in read else 0
    return Progress(done, entry.pages)


# 📚 Fold the trees: add up the progress of every entry.
# 木を畳む：すべての項目の進み具合を足し合わせる
def progress_of(entries: list[Entry], read: set[int]) -> Progress:
    every = (e for tree in entries for e in walk(tree))
    return sum((own(e, read) for e in every), Progress(0, 0))
# ANCHOR_END: functional


# The sample book, "Dragonology for Beginners".
# サンプルの本『はじめてのドラゴン学』
chapter1 = Entry(2, "第1章 見分け方", 4, (
    Entry(3, "1.1 うろこの色", 8),
    Entry(4, "1.2 火を吐くもの", 10),
))
part1 = Entry(1, "第1部 ドラゴンを知る", 2, (
    chapter1,
    Entry(5, "第2章 すみかと季節", 24),
))
part2 = Entry(6, "第2部 ドラゴンと話す", 2, (
    Entry(7, "第3章 あいさつの作法", 16),
    Entry(8, "第4章 怒らせたときは", 12),
))
book = [part1, part2]
read = {1, 2, 3, 4, 5}


# The procedural style reads plain dicts.
# 手続き型スタイルは、ただの dict を読む
def to_dict(e: Entry) -> dict:
    return {"no": e.no, "title": e.title, "pages": e.pages,
            "children": [to_dict(c) for c in e.children]}


def both(entries: list[Entry], read: set[int]) -> tuple[int, int]:
    a = progress([to_dict(e) for e in entries], read)
    b = progress_of(entries, read)
    assert a == (b.done, b.total), (a, b)
    assert percent(*a) == b.percent()
    return a


assert both(book, read) == (48, 78)
assert percent(48, 78) == 62
assert both([part1], read) == (48, 48) and percent(48, 48) == 100
assert both([part2], read) == (0, 30) and percent(0, 30) == 0
assert both([chapter1], read) == (22, 22)
assert both([], read) == (0, 0)
assert both([Entry(1, "あとがき", 3)], {1}) == (3, 3)
assert both([Entry(1, "あとがき", 3)], set()) == (0, 3)
assert both(book, set()) == (0, 78)
assert both(book, set(range(1, 9))) == (78, 78)
assert [n for n in (e.no for e in walk(part1))] == [1, 2, 3, 4, 5]
for (d, t), p in [((48, 78), 62), ((1, 1000), 1), ((999, 1000), 99),
                  ((0, 30), 0), ((30, 30), 100), ((0, 0), 0),
                  ((1, 2), 50), ((1, 8), 13)]:
    assert percent(d, t) == p, (d, t)
    assert Progress(d, t).percent() == p, (d, t)
