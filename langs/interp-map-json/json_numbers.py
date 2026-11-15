# The Python code in the chapter on Map and JSON: the numbers of JSON.
# Map と JSON の章の Python のコード：JSON の数
import json
from decimal import Decimal

prices_text = "[0.1, 0.2]"

# ANCHOR: procedural
# Python (procedural style): json.loads reads 0.1 as a float
# Python（手続き型スタイル）：json.loads は 0.1 を float で読む
prices = json.loads(prices_text)
total = 0
for p in prices:
    total = total + p
print(total)  # 0.30000000000000004
# ANCHOR_END: procedural

assert total == 0.30000000000000004
assert total != 0.3

# ANCHOR: functional
# Python (functional style): read the fractions as Decimal instead
# Python（関数型スタイル）：小数を、代わりに Decimal で読む
exact = json.loads(prices_text, parse_float=Decimal)
print(sum(exact))  # 0.3
# ANCHOR_END: functional

assert sum(exact) == Decimal("0.3")
assert str(sum(exact)) == "0.3"

# A big whole number stays exact: Python's int has no limit.
# 大きな整数は、ぴったりのまま。Python の int には上限がない
assert json.loads("12345678901234567890") == 12345678901234567890
