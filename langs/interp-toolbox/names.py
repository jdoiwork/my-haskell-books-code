# The downloads of the chapter, sorted into kinds, in Python: str.lower,
# "sep".join, splitlines, itertools.groupby, and a slice for splitAt.
# 章のダウンロードの仕分けを Python で。str.lower・"区切り".join・
# splitlines・itertools.groupby、splitAt の代わりにスライス
from itertools import groupby
from pathlib import PurePath

downloads = "b.PDF\nphoto.png\na.pdf\nnotes.TXT\nc.Png\ntodo.txt\n"


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def kinds_procedural(names):
    groups = {}
    for name in names:
        kind = PurePath(name).suffix[1:].lower()
        if kind not in groups:
            groups[kind] = []
        groups[kind].append(name)
    lines = []
    for kind in sorted(groups):
        lines.append(kind + ": " + ", ".join(groups[kind]))
    return lines
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def kind(name):
    return PurePath(name).suffix[1:].lower()


def kinds_functional(names):
    return [f"{k}: {', '.join(g)}"
            for k, g in groupby(sorted(names, key=kind), key=kind)]
# ANCHOR_END: functional


# ANCHOR: groupby
# groupby groups only the neighbors, as Lam's groupBy does
# groupby がまとめるのは、となりどうしだけ（Lam の groupBy と同じ）
neighbors = [[k for k in g] for _, g in groupby([1, 1, 2, 1])]
# ANCHOR_END: groupby

names = downloads.splitlines()
want = ["pdf: b.PDF, a.pdf", "png: photo.png, c.Png",
        "txt: notes.TXT, todo.txt"]
assert kinds_procedural(names) == want
assert kinds_functional(names) == want
assert neighbors == [[1, 1], [2], [1]]
first, rest = names[:2], names[2:]
assert first == ["b.PDF", "photo.png"] and len(rest) == 4
assert "done".upper() == "DONE"
print("ok")
