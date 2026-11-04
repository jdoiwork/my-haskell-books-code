# The guessing game and log2 of the chapter "Exponents and logarithms",
# in Python.
# 「指数と対数」の章の数当てと log2 を、Python で
import bisect
import math

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def guesses(lo, hi, target):
    said = []
    while lo <= hi:
        mid = (lo + hi) // 2
        said.append(mid)
        if mid == target:
            break
        elif mid < target:
            lo = mid + 1
        else:
            hi = mid - 1
    return said
# ANCHOR_END: procedural

assert guesses(1, 1000, 777) == [500, 750, 875, 812, 781, 765, 773, 777]

# ANCHOR: functional
# Python (functional style: the library does the halving)
# Python（関数型スタイル：半分にするのはライブラリ）
levels = [100, 200, 400, 800, 1600, 3200]
where = bisect.bisect_right(levels, 1000)   # 4: 1000 is after 800
halvings = (1000).bit_length() - 1          # 9, like floor(log2 1000)
exact = math.log2(1000)                     # 9.965784284662087
# ANCHOR_END: functional

assert where == 4 and halvings == 9
assert exact == 9.965784284662087
print("ok")
