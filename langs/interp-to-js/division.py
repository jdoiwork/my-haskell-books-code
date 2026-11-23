# The Python code in the chapter "Turning Lam into JavaScript".
# 「JavaScript に変換する」の章の Python のコード
import math

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def factorial(n):
    total = 1
    for i in range(1, n + 1):
        total *= i
    return total             # an int never runs out of digits
# ANCHOR_END: procedural

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def factorial2(n):
    return math.prod(range(1, n + 1))

halves = (-7 // 2, -7 % 2)   # (-4, 1): // rounds down
# ANCHOR_END: functional

assert factorial(25) == 15511210043330985984000000
assert factorial2(25) == factorial(25)
assert halves == (-4, 1)
assert int(-7 / 2) == -3     # toward 0, as JavaScript's BigInt /
print(factorial(25), halves)
