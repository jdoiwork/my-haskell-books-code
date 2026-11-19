# The Python code in the matcher chapter of MiniRegex: which branch wins,
# greedy and lazy, and a tiny matcher written with a generator (the list
# of candidates, made one at a time).
# MiniRegex のマッチャーの章の Python のコード：どちらの枝が勝つか、欲ばりと
# 控えめ、ジェネレーターで書いた小さなマッチャー
# （候補のリストを、1つずつ作る）
import re
import subprocess

# ANCHOR: first
# Python: the left branch wins, even when the right one is longer
# Python：右の枝のほうが長くても、左の枝が勝つ
print(re.search(r"a|ab", "abc").group())
print(re.search(r"ab|a", "abc").group())
print(re.search(r"a*?b", "aaab").group())
# ANCHOR_END: first

assert re.search(r"a|ab", "abc").group() == "a"
assert re.search(r"ab|a", "abc").group() == "ab"
assert re.search(r"a*?b", "aaab").group() == "aaab"

# The same with awk, which follows POSIX: the longest at the leftmost place
# awk でも。awk は POSIX に従う：いちばん左の場所で、いちばん長いもの
awk = subprocess.run(
    ["awk", "{ if (match($0, /a|ab/)) print substr($0, RSTART, RLENGTH) }"],
    input="abc\n", capture_output=True, text=True, check=True).stdout
assert awk == "ab\n", awk


# ANCHOR: generator
# Python: a matcher that yields every place where the tree can end.
#   The tree is made of tuples: ("lit", "a"), ("seq", [...]),
#   ("alt", [...]) and ("star", tree).
# Python：木が終われる場所を、1つずつ yield するマッチャー。木はタプルで
#   ("lit", "a")・("seq", [...])・("alt", [...])・("star", 木)
def ends(tree, s, i):
    kind = tree[0]
    if kind == "lit":
        if s.startswith(tree[1], i):
            yield i + 1
    elif kind == "seq":
        yield from seq(tree[1], s, i)
    elif kind == "alt":
        for branch in tree[1]:
            yield from ends(branch, s, i)
    elif kind == "star":
        for j in ends(tree[1], s, i):
            if j == i:
                yield j
            else:
                yield from ends(tree, s, j)
        yield i


def seq(parts, s, i):
    if not parts:
        yield i
        return
    for j in ends(parts[0], s, i):
        yield from seq(parts[1:], s, j)


# (a|ab)*c
tree = ("seq", [("star", ("alt", [("lit", "a"),
                                  ("seq", [("lit", "a"), ("lit", "b")])])),
                ("lit", "c")])
print(next(ends(tree, "ababc", 0)))
# ANCHOR_END: generator

assert next(ends(tree, "ababc", 0)) == 5
assert list(ends(("star", ("lit", "a")), "aab", 0)) == [2, 1, 0]
