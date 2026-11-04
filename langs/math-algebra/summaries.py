# Combinable summaries of the chapter "Associativity and monoids", in Python.
# 「結合法則とモノイド」の章の、合わせられるまとめを、Python で
from collections import Counter
from functools import reduce

servers = [[10, 20], [30, 400], [15]]   # the logs of three servers

# ANCHOR: procedural
# Python (procedural style: keep the count and the total)
# Python（手続き型スタイル：個数と合計を持つ）
count, total = 0, 0
for log in servers:
    for t in log:
        count += 1
        total += t
mean = total / count                     # 95.0
# ANCHOR_END: procedural
assert mean == 95.0


# ANCHOR: functional
# Python (functional style: summaries and a combine function)
# Python（関数型スタイル：まとめと、合わせる関数）
def combine(a, b):
    return (a[0] + b[0], a[1] + b[1])     # (count, total)

empty = (0, 0)                            # changes nothing / 何も変えない
per_server = [(len(log), sum(log)) for log in servers]
count, total = reduce(combine, per_server, empty)
words = Counter("fire ice".split()) + Counter("fire".split())
# ANCHOR_END: functional
assert total / count == 95.0
assert words == Counter({"fire": 2, "ice": 1})
means = [sum(log) / len(log) for log in servers]
assert abs(sum(means) / len(means) - 81.66666666666667) < 1e-9
print("ok")
