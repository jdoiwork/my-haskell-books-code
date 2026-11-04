# The best run of rooms, for the chapter "Start from the specification".
# いちばんよい連続した部屋。「仕様から始める」の章
from itertools import accumulate

dungeon = [3, -5, 4, -1, 2, 6, -8, 5, -2]


# ANCHOR: procedural
# Python (procedural style: Kadane's algorithm with two variables)
# Python（手続き型スタイル：2つの変数で、カデインのアルゴリズム）
def best_run(rooms):
    best = 0
    here = 0
    for x in rooms:
        here = max(0, here + x)
        best = max(best, here)
    return best
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: the running "best ending here" with accumulate)
# Python（関数型スタイル：accumulate で「ここで終わる最大」を累計）
def best_run_f(rooms):
    endings = accumulate(rooms, lambda here, x: max(0, here + x),
                         initial=0)
    return max(endings)
# ANCHOR_END: functional


def best_run_spec(rooms):
    n = len(rooms)
    return max(sum(rooms[i:j]) for i in range(n + 1) for j in range(i, n + 1))


assert best_run(dungeon) == best_run_f(dungeon) == best_run_spec(dungeon) == 11
for k in range(200):
    rooms = [(i * 7919 + k) % 21 - 10 for i in range(k % 13)]
    assert best_run(rooms) == best_run_f(rooms) == best_run_spec(rooms)
print("ok")
