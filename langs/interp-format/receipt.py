# The Python code in the chapter on f"...": one line of the receipt,
# first joined with +, then with an f-string.
# f"..." の章の Python のコード：レシートの1行を、まず + でつなぎ、
# 次に f-string で書く

prices = {"sticker": 300, "mug": 1200, "tshirt": 2500}
orders = [("sticker", 4), ("mug", 1), ("tshirt", 3)]


# ANCHOR: procedural
# Python (procedural style): str() and +, and the spaces by hand
# Python（手続き型スタイル）：str() と + で。空白も手で数える
def line_plus(item, n):
    price = prices[item]
    count = str(n)
    total = str(n * price)
    return (item + " " * (10 - len(item))
            + " " * (3 - len(count)) + count
            + " x " + str(price) + " = " + total)
# ANCHOR_END: procedural


# ANCHOR: fstring
# Python (f-string): the value and its format, in the string
# Python（f-string）：値と書式を、文字列の中に
def line(item, n):
    price = prices[item]
    return f"{item:<10}{n:>3} x {price:>6,} = {n * price:>9,}"
# ANCHOR_END: fstring


assert line_plus("mug", 1) == "mug         1 x 1200 = 1200"
assert [line(i, n) for i, n in orders] == [
    "sticker     4 x    300 =     1,200",
    "mug         1 x  1,200 =     1,200",
    "tshirt      3 x  2,500 =     7,500",
]


# ANCHOR: lateError
# Python: a wrong format is found only when the line runs
# Python：書式のまちがいは、その行が動いたときに見つかる
def name_line(name):
    return f"{name:,}"       # ',' cannot be used with a string


print("defined name_line")   # Python is happy so far / ここまでは平気
try:
    name_line("Lam")
except ValueError as e:
    print("ValueError:", e)
# ANCHOR_END: lateError

try:
    name_line("Lam")
    assert False
except ValueError as e:
    assert str(e) == "Cannot specify ',' with 's'."
