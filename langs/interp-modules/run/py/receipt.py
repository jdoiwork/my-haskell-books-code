# ANCHOR: all
# Python: receipt.py, next to price.py
# Python：receipt.py（price.py の隣）
import price as p             # p.from_yen  (like  import Price as P)
from price import show        # show        (like  exposing (show))

prices = [p.from_yen(1200), p.from_yen(350), p.from_yen(-5)]
for m in prices:
    print(show(m) if m is not None else "not a price")

# Nothing stops this: Python trusts you.
# これを止めるものはない。Python は、あなたを信じる
cheat = p.Price(-5)
print(show(cheat))
# ANCHOR_END: all
