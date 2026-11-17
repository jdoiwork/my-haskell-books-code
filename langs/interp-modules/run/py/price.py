# ANCHOR: all
# Python (a module): price.py
# Python（モジュール）：price.py
# The names that `from price import *` takes. The leading _ says
# "inside only", but it is only a promise between people.
# `from price import *` が持ってくる名前。頭の _ は「中だけで使う」の
# 印だが、人どうしの約束にすぎない
__all__ = ["Price", "from_yen", "add", "show"]


class Price:
    def __init__(self, yen: int) -> None:
        self._yen = yen


def from_yen(n: int) -> Price | None:
    return None if n < 0 else Price(n)


def add(a: Price, b: Price) -> Price:
    return Price(a._yen + b._yen)


def show(p: Price) -> str:
    return f"{p._yen:,} yen"
# ANCHOR_END: all
