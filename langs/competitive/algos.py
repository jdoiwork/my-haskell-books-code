# The Python code in the competitive programming chapter.
# 競技プログラミングの章の Python のコード
from collections import deque
from itertools import accumulate


# ANCHOR: prefix_procedural
# Python (procedural style: prefix sums with a for loop)
# Python（手続き型スタイル：for 文で累積和）
def prefix_sums(damages):
    sums = [0]
    for d in damages:
        sums.append(sums[-1] + d)
    return sums
# ANCHOR_END: prefix_procedural


# ANCHOR: prefix_functional
# Python (functional style: accumulate)
# Python（関数型スタイル：accumulate）
def prefix_sums_acc(damages):
    return list(accumulate(damages, initial=0))


def range_sum(sums, l, r):
    return sums[r] - sums[l - 1]
# ANCHOR_END: prefix_functional

sums = prefix_sums([3, 1, 4, 1, 5])
assert sums == prefix_sums_acc([3, 1, 4, 1, 5]) == [0, 3, 4, 8, 9, 14]
assert range_sum(sums, 2, 4) == 6


# ANCHOR: binary
# Python (binary search on the answer)
# Python（答えで二分探索）
def turns_needed(hps, attack):
    return sum((hp + attack - 1) // attack for hp in hps)


def weakest_attack(hps, k):
    lo, hi = 0, max(hps)  # lo: not enough, hi: enough
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if turns_needed(hps, mid) <= k:
            hi = mid
        else:
            lo = mid
    return hi
# ANCHOR_END: binary

assert weakest_attack([10, 7, 3], 5) == 5


# ANCHOR: dp
# Python (dynamic programming: best[w] is the most value with room w)
# Python（動的計画法：best[w] は、重さ w までに入る価値の最大）
def best_bag(capacity, items):
    best = [0] * (capacity + 1)
    for weight, value in items:
        for w in range(capacity, weight - 1, -1):
            best[w] = max(best[w], best[w - weight] + value)
    return best[capacity]
# ANCHOR_END: dp

assert best_bag(8, [(3, 30), (4, 50), (5, 60)]) == 90


# ANCHOR: bfs
# Python (BFS with deque)
# Python（deque で幅優先探索）
def shortest_path(rows):
    h, w = len(rows), len(rows[0])
    dist = [[-1] * w for _ in range(h)]
    dist[0][0] = 0
    queue = deque([(0, 0)])
    while queue:
        r, c = queue.popleft()
        for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            nr, nc = r + dr, c + dc
            if (0 <= nr < h and 0 <= nc < w and rows[nr][nc] != "#"
                    and dist[nr][nc] < 0):
                dist[nr][nc] = dist[r][c] + 1
                queue.append((nr, nc))
    return dist[h - 1][w - 1]
# ANCHOR_END: bfs

assert shortest_path(["....", ".##.", "...#", "#..."]) == 6
assert shortest_path([".#", "#."]) == -1


# ANCHOR: twosum
# Python (LeetCode 1, Two Sum: remember what we have seen in a dict)
# Python（LeetCode の1問目 Two Sum：見たものを dict に覚える）
def two_sum(nums, target):
    seen = {}
    for i, x in enumerate(nums):
        if target - x in seen:
            return [seen[target - x], i]
        seen[x] = i
    return None
# ANCHOR_END: twosum

assert two_sum([2, 7, 11, 15], 9) == [0, 1]
assert two_sum([3, 2, 4], 6) == [1, 2]
assert two_sum([1, 2], 10) is None
print("ok")
