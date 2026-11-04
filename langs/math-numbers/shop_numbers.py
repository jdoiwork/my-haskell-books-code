# The numbers of the chapter "What numbers really are", in Python.
# 「数の正体」の章の数を、Python で
from decimal import Decimal

# ANCHOR: numbers
# Python
big = 2 ** 100                 # int never overflows / int はあふれない
almost = 0.1 + 0.2             # 0.30000000000000004
floor_div = -7 // 2            # -4  (like div)
floor_mod = -7 % 2             # 1   (like mod)
half = round(2.5)              # 2   (to the even one / 偶数へ)
exact = Decimal("0.1") + Decimal("0.2")   # Decimal('0.3')
# ANCHOR_END: numbers

assert big == 1267650600228229401496703205376
assert almost == 0.30000000000000004 and almost != 0.3
assert (floor_div, floor_mod) == (-4, 1)
assert half == 2 and round(3.5) == 4
assert exact == Decimal("0.3")
print("ok")
