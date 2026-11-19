# ANCHOR: all
# Python: menu.py, next to price.py
# Python：menu.py（price.py の隣）
# The two lines together are Lam's  import Price as P (fromYen).
# 2行で、Lam の  import Price as P (fromYen)  と同じ
import price as p             # p.from_yen, p.add
from price import from_yen    # from_yen

prices = [from_yen(1200), from_yen(350), p.from_yen(-5)]
for m in prices:
    print(f"{m}" if m is not None else "not a price")

# Nothing stops this: Python trusts you.
# これを止めるものはない。Python は、あなたを信じる
cheat = p.Price(-5)
print(f"{cheat}")
# ANCHOR_END: all
