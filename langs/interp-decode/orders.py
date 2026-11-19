# The orders of the web shop, as in interp-decode.md: read JSON into
# records, and say every problem with its place.
# 「データを読む」の章の、ウェブのお店の注文。JSON をレコードに読み、
# 問題を場所つきで全部言う
import csv
import io
import json
import tomllib

from pydantic import BaseModel, ValidationError

BAD = """
{"orders": [
  {"id": 1, "item": "sticker", "count": "four", "price": 300.5},
  {"id": 2, "item": "mug", "count": 1, "price": 1200},
  {"id": 3, "name": "T-shirt, large", "count": 2}
]}
"""


# ANCHOR: procedural
# Python (procedural style): look at each field, and keep a list of
# the problems
# Python（手続き型スタイル）：項目を1つずつ見て、問題をリストにためる
def read_orders(doc):
    orders = []
    problems = []
    for i, o in enumerate(doc["orders"]):
        ok = True
        for key, kind in [("item", str), ("count", int), ("price", int)]:
            if key not in o:
                problems.append(f".orders[{i}]: missing field {key!r}")
                ok = False
            elif type(o[key]) is not kind:
                problems.append(f".orders[{i}].{key}: not {kind.__name__}")
                ok = False
        if ok:
            orders.append((o["item"], o["count"], o["price"]))
    return orders, problems
# ANCHOR_END: procedural


orders, problems = read_orders(json.loads(BAD))
for p in problems:
    print(p)
assert orders == [("mug", 1, 1200)]
assert len(problems) == 4


# ANCHOR: pydantic
# Python (functional style): pydantic reads the shape from the class,
# and gives back every problem at once
# Python（関数型スタイル）：pydantic がクラスから形を読み、問題を
# まとめて全部返す
class Order(BaseModel, strict=True):
    item: str
    count: int
    price: int


class Orders(BaseModel, strict=True):
    orders: list[Order]


try:
    Orders.model_validate_json(BAD)
except ValidationError as e:
    for err in e.errors():
        print(err["loc"], err["msg"])
# ANCHOR_END: pydantic

try:
    Orders.model_validate_json(BAD)
    assert False
except ValidationError as e:
    assert [err["loc"] for err in e.errors()] == [
        ("orders", 0, "count"), ("orders", 0, "price"),
        ("orders", 2, "item"), ("orders", 2, "price")]


# ANCHOR: csvToml
# Python: csv.DictReader gives every field as a string; int() reads a
# number. tomllib (in the standard library since 3.11) reads TOML.
# Python：csv.DictReader はフィールドを全部文字で返す。数は int() で
# 読む。TOML は tomllib（3.11 から標準ライブラリ）
REGISTER = 'time,item,count,price\n10:05,"T-shirt, large",1,2500\n'
for row in csv.DictReader(io.StringIO(REGISTER)):
    print(row["item"], int(row["count"]) * int(row["price"]))

SHOP = '[shop]\nname = "Lam Shop"\nopened = 2026-11-01\n'
shop = tomllib.loads(SHOP)["shop"]
print(shop["name"], shop["opened"])
# ANCHOR_END: csvToml

assert list(csv.DictReader(io.StringIO(REGISTER)))[0]["count"] == "1"
assert str(shop["opened"]) == "2026-11-01"
