# Kruskal's algorithm with union-find, for the chapter "Graph algorithms".
# ユニオン・ファインドでクラスカル法。「グラフのアルゴリズム」の章
from functools import reduce

roads = [("Haskelia", "Pyland", 7), ("Haskelia", "Rustburg", 5),
         ("Pyland", "Rustburg", 8), ("Pyland", "Tsville", 9),
         ("Rustburg", "Tsville", 7), ("Rustburg", "Rubyport", 6),
         ("Tsville", "Rubyport", 8)]


# ANCHOR: procedural
# Python (procedural style: a parent dict, changed in place)
# Python（手続き型スタイル：親の辞書を、その場で書き換える）
def kruskal(rs):
    parent = {}

    def find(town):
        while parent.get(town, town) != town:
            town = parent[town]
        return town

    built = []
    for a, b, cost in sorted(rs, key=lambda r: r[2]):
        ra, rb = find(a), find(b)
        if ra != rb:
            parent[ra] = rb
            built.append((a, b, cost))
    return built
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: reduce, with a new parent dict each time)
# Python（関数型スタイル：reduce で、毎回新しい親の辞書を作る）
def find_in(parent, town):
    return town if parent.get(town, town) == town \
        else find_in(parent, parent[town])


def kruskal_f(rs):
    def build(acc, road):
        parent, built = acc
        a, b, _ = road
        ra, rb = find_in(parent, a), find_in(parent, b)
        if ra == rb:
            return acc
        return ({**parent, ra: rb}, built + [road])
    return reduce(build, sorted(rs, key=lambda r: r[2]), ({}, []))[1]
# ANCHOR_END: functional


assert sum(c for _, _, c in kruskal(roads)) == 25
assert kruskal(roads) == kruskal_f(roads)
print("ok")
