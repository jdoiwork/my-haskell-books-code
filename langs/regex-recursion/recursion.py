# The Python code in the chapter on recursion of MiniRegex: Python's re
# has no (?R), so the notes in the notes are found by counting how deep
# we are, by hand.
# MiniRegex の再帰の章の Python のコード：Python の re には (?R) がない
# ので、入れ子の注記は、いまの深さを手で数えて探す
from itertools import accumulate
import re

memo = "Mira (healer (2nd class) from Lam (east)) paid 120 gold (cash)"

# ANCHOR: flat
# Python: re finds only the innermost notes
# Python：re で探せるのは、いちばん内側の注記だけ
print(re.findall(r"\([^()]*\)", memo))
# ANCHOR_END: flat

assert re.findall(r"\([^()]*\)", memo) == ["(2nd class)", "(east)", "(cash)"]

# ANCHOR: procedural
# Python (procedural style): count how deep we are, and keep where a
# note at depth 1 starts
# Python（手続き型スタイル）：深さを数え、深さ 1 の注記が始まる場所を
# 覚えておく
notes = []
depth = 0
start = 0
for i, c in enumerate(memo):
    if c == "(":
        if depth == 0:
            start = i
        depth += 1
    elif c == ")" and depth > 0:
        depth -= 1
        if depth == 0:
            notes.append(memo[start:i + 1])
print(notes)
# ANCHOR_END: procedural

assert notes == ["(healer (2nd class) from Lam (east))", "(cash)"]

# ANCHOR: functional
# Python (functional style): the depth after each character, with
# accumulate; a note starts where the depth goes from 0 to 1
# Python（関数型スタイル）：accumulate で、文字ごとのあとの深さ。
# 注記は、深さが 0 から 1 になる所で始まり、1 から 0 になる所で終わる
steps = [{"(": 1, ")": -1}.get(c, 0) for c in memo]
depths = list(accumulate(steps, initial=0))
pairs = list(zip(depths, depths[1:]))
opens = [i for i, d in enumerate(pairs) if d == (0, 1)]
closes = [i for i, d in enumerate(pairs) if d == (1, 0)]
print([memo[a:b + 1] for a, b in zip(opens, closes)])
# ANCHOR_END: functional

assert [memo[a:b + 1] for a, b in zip(opens, closes)] == notes
