# The Python code in the chapter on data and case.
# data と case の章の Python のコード
from dataclasses import dataclass
from typing import assert_never


# ANCHOR: procedural
# Python (procedural style): one class per shape, and isinstance
# Python（手続き型スタイル）：図形ごとのクラスと、isinstance
@dataclass
class Circle:
    r: int


@dataclass
class Rect:
    w: int
    h: int


def area(s: Circle | Rect) -> int:
    if isinstance(s, Circle):
        return 3 * s.r * s.r
    elif isinstance(s, Rect):
        return s.w * s.h
    else:
        raise ValueError(s)
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style): match, with patterns and guards
# Python（関数型スタイル）：match と、パターンとガード
type Shape = Circle | Rect


def name(s: Shape) -> str:
    match s:
        case Rect(w, h) if w == h:
            return "square"
        case Rect(_, _):
            return "rect"
        case Circle(_):
            return "circle"
        case _:
            assert_never(s)
# ANCHOR_END: functional


assert area(Circle(2)) == 12
assert area(Rect(3, 4)) == 12
shapes: list[Shape] = [Rect(3, 3), Rect(2, 5), Circle(1)]
assert [name(s) for s in shapes] == ["square", "rect", "circle"]
