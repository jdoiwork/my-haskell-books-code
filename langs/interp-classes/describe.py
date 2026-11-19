# The Python code in the chapter on classes and instances.
# 型クラスを作るの章の Python のコード
from dataclasses import dataclass
from functools import singledispatch


@dataclass
class Circle:
    radius: int


@dataclass
class Rect:
    width: int
    height: int


@dataclass
class Yen:
    amount: int

    # + for money: sum() uses it.
    # お金の +。sum() が使う
    def __add__(self, other):
        return Yen(self.amount + other.amount)


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def describe(x):
    if isinstance(x, Circle):
        return f"a circle of radius {x.radius}"
    elif isinstance(x, Rect):
        return f"a {x.width}x{x.height} rect"
    elif isinstance(x, Yen):
        return f"{x.amount} yen"
    raise TypeError(f"cannot describe {x!r}")
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: one function, chosen by the type of the
#   first argument as the program runs)
# Python（関数型スタイル：最初の引数の型で、動かすときに選ぶ）
@singledispatch
def describe2(x):
    raise TypeError(f"cannot describe {x!r}")

@describe2.register
def _(s: Circle):
    return f"a circle of radius {s.radius}"

@describe2.register
def _(r: Rect):
    return f"a {r.width}x{r.height} rect"

@describe2.register
def _(m: Yen):
    return f"{m.amount} yen"

def twice(x):
    return describe2(x) + ", " + describe2(x)
# ANCHOR_END: functional


# ANCHOR: empty
# Python (no argument, no type to choose with: the empty is passed in)
# Python（引数がなければ、選ぶための型がない。empty を渡す）
def total(xs, empty):
    result = empty
    for x in xs:
        result = result + x
    return result

money = total([Yen(300), Yen(450)], Yen(0))     # Yen(amount=750)
nothing = sum([], Yen(0))                         # Yen(amount=0)
# ANCHOR_END: empty

assert describe(Circle(2)) == "a circle of radius 2"
assert describe(Yen(500)) == "500 yen"
assert describe2(Rect(3, 5)) == "a 3x5 rect"
assert twice(Yen(80)) == "80 yen, 80 yen"
assert money == Yen(750) and nothing == Yen(0)
assert sum([Yen(1), Yen(2)], Yen(0)) == Yen(3)
try:
    describe2(30)
    assert False
except TypeError as e:
    assert str(e) == "cannot describe 30"
print("ok")
