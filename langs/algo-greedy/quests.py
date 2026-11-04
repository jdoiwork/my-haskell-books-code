# The most quests without overlap, for the chapter "Greedy algorithms".
# 重ならない依頼を、いちばん多く。「貪欲法」の章
from functools import reduce

quests = [("dragon", 1, 9), ("herbs", 2, 4), ("goblins", 4, 6),
          ("escort", 5, 8), ("slimes", 6, 7), ("letter", 8, 10)]


# ANCHOR: procedural
# Python (procedural style: sort by end, keep the end of the last one taken)
# Python（手続き型スタイル：終わる順に並べ、最後に取った依頼の終わりを覚える）
def most_quests(qs):
    taken = []
    last_end = None
    for name, start, end in sorted(qs, key=lambda q: q[2]):
        if last_end is None or start >= last_end:
            taken.append(name)
            last_end = end
    return taken
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: reduce, carrying the names and the last end)
# Python（関数型スタイル：reduce で、名前と最後の終わりを持ち回る）
def most_quests_f(qs):
    def take(acc, q):
        names, last_end = acc
        name, start, end = q
        return (names + [name], end) if start >= last_end else acc
    names, _ = reduce(take, sorted(qs, key=lambda q: q[2]), ([], -1))
    return names
# ANCHOR_END: functional


expected = ["herbs", "goblins", "slimes", "letter"]
assert most_quests(quests) == most_quests_f(quests) == expected
print("ok")
