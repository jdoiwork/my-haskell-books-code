# Add 1 to n, n times over, by a function that calls itself last.
# 最後に自分を呼ぶ関数で、n から 1 までを足す
import sys


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def count_down(n, total):
    if n == 0:
        return total
    return count_down(n - 1, total + n)


# The same, as a loop: the arguments get new values, and around again.
# 同じものを、ループで：引数に新しい値を入れて、もう一周
def count_down_loop(n, total):
    while True:
        if n == 0:
            return total
        n, total = n - 1, total + n
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
from functools import reduce

total = reduce(lambda acc, k: acc + k, range(1, 1_000_001), 0)
# ANCHOR_END: functional

assert sys.getrecursionlimit() == 1000
assert count_down(900, 0) == 405450
try:
    count_down(1_000_000, 0)
    assert False
except RecursionError:
    pass
assert count_down_loop(1_000_000, 0) == 500000500000
assert total == 500000500000
print("ok")
