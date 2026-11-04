# The graphs of the chapter "Graphs", in Python.
# 「グラフ」の章のグラフを、Python で
import heapq
from collections import deque
from graphlib import CycleError, TopologicalSorter

roads = [("Start", "River", 2), ("Start", "Forest", 7),
         ("River", "Forest", 3), ("Forest", "Castle", 4),
         ("River", "Mountain", 6), ("Mountain", "Castle", 2)]
graph = {}
for a, b, h in roads:
    graph.setdefault(a, []).append((b, h))
    graph.setdefault(b, []).append((a, h))

# ANCHOR: procedural
# Python (procedural style: breadth-first with a queue)
# Python（手続き型スタイル：列を使う幅優先）
hops = {"Start": 0}
queue = deque(["Start"])
while queue:
    town = queue.popleft()
    for nxt, _ in graph[town]:
        if nxt not in hops:
            hops[nxt] = hops[town] + 1
            queue.append(nxt)
# ANCHOR_END: procedural
assert hops == {"Start": 0, "River": 1, "Forest": 1,
                "Mountain": 2, "Castle": 2}

# ANCHOR: functional
# Python (the standard library: a heap, and graphlib)
# Python（標準ライブラリ：ヒープと graphlib）
def dijkstra(start):
    done, frontier = {}, [(0, start)]
    while frontier:
        d, town = heapq.heappop(frontier)   # the nearest / いちばん近い
        if town in done:
            continue
        done[town] = d
        for nxt, h in graph[town]:
            if nxt not in done:
                heapq.heappush(frontier, (d + h, nxt))
    return done

needs = {"fire": ["magic basics"], "ice": ["magic basics"],
         "steam": ["fire", "ice"], "fire sword": ["fire", "sword"]}
order = list(TopologicalSorter(needs).static_order())
# ANCHOR_END: functional
assert dijkstra("Start")["Castle"] == 9
assert all(order.index(n) < order.index(s)
           for s, ns in needs.items() for n in ns)
try:
    list(TopologicalSorter({"a": ["b"], "b": ["a"]}).static_order())
    raise AssertionError("a cycle should be found")
except CycleError:
    pass
print("ok")
