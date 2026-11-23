# The household account of the chapter, in Python: csv.DictReader and
# its line_num, and json.dumps with default= for a type of our own.
# 章の家計簿を Python で。csv.DictReader とその line_num、自分の型には
# json.dumps の default=
import csv
import io
import json
from enum import Enum

good = """date,category,amount,memo
2026-11-01,food,1200,lunch
2026-11-02,books,3300,"Haskell, 2nd"
2026-11-03,food,800,
2026-11-05,transport,450,"bus
to the workshop"
2026-11-07,food,650,snack
"""
bad = good.replace("food,650,snack", "food,abc,typo")


class Category(Enum):
    FOOD = "food"
    BOOKS = "books"
    TRANSPORT = "transport"


# ANCHOR: lineNum
# Python (procedural style)
# Python（手続き型スタイル）
def totals(text):
    reader = csv.DictReader(io.StringIO(text))
    sums = {}
    problems = []
    for row in reader:
        try:
            amount = int(row["amount"])
        except ValueError:
            # line_num: the lines read so far (the row ends there)
            # line_num：ここまでに読んだ行の数（行は、そこで終わる）
            problems.append(f"line {reader.line_num}, column amount: "
                            f"not an int: {row['amount']!r}")
            continue
        c = Category(row["category"])
        sums[c] = sums.get(c, 0) + amount
    return sums, problems
# ANCHOR_END: lineNum


# ANCHOR: dumps
# A key of an Enum is not a str: json.dumps needs our own help.
# Enum のキーは str ではないので、json.dumps には手助けが要る
def to_json(sums):
    return json.dumps({c.value: n for c, n in sorted(
        sums.items(), key=lambda kv: kv[0].value)}, separators=(",", ":"))


def default(value):
    if isinstance(value, Category):
        return value.value
    raise TypeError(f"not JSON: {value!r}")
# ANCHOR_END: dumps


sums, problems = totals(good)
assert problems == []
assert to_json(sums) == '{"books":3300,"food":2650,"transport":450}'
assert json.dumps([Category.FOOD], default=default) == '["food"]'
# default= helps only the values: a key of an Enum is still a TypeError.
# default= が手伝うのは値だけ。Enum のキーは、やはり TypeError
try:
    json.dumps({Category.FOOD: 1}, default=default)
    assert False
except TypeError:
    pass
# The memo of the bus has a line break: the bad row is line 7 of the
# file, and line_num says 7 (the line it ends on).
# バスのメモに改行がある。まちがいの行はファイルの7行目で、line_num も 7
_, problems = totals(bad)
assert problems == ["line 7, column amount: not an int: 'abc'"], problems
# For the row of the bus (lines 5 to 6), line_num is 6: where it ends.
# バスの行（5〜6行目）の line_num は 6：終わる行
reader = csv.DictReader(io.StringIO(good))
ends = [reader.line_num for _ in reader]
assert ends == [2, 3, 4, 6, 7], ends
print("ok")
