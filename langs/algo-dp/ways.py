# Ways to pay, for the chapter "Dynamic programming".
# 払い方の数。「動的計画法」の章
from functools import lru_cache

COINS = [1, 5, 10, 25]


# ANCHOR: procedural
# Python (procedural style: fill the table from the bottom, in loops)
# Python（手続き型スタイル：表を下から、ループで埋める）
def ways(coins, amount):
    table = [1] + [0] * amount
    for c in coins:
        for a in range(c, amount + 1):
            table[a] += table[a - c]
    return table[amount]
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: the spec's recursion, remembered by lru_cache)
# Python（関数型スタイル：仕様の再帰を、lru_cache に覚えてもらう）
def ways_memo(coins, amount):
    @lru_cache(maxsize=None)
    def go(k, a):
        if a == 0:
            return 1
        if k == 0 or a < 0:
            return 0
        return go(k - 1, a) + go(k, a - coins[k - 1])
    return go(len(coins), amount)
# ANCHOR_END: functional


assert ways(COINS, 100) == ways_memo(tuple(COINS), 100) == 242
for a in range(60):
    assert ways(COINS, a) == ways_memo(tuple(COINS), a)
print("ok")
