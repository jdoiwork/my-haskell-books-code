# Folding an expression tree, for the chapter "Recursion schemes".
# 式の木を畳む。「再帰スキーム」の章
from dataclasses import dataclass


@dataclass
class Lit:
    n: int


@dataclass
class Add:
    a: object
    b: object


@dataclass
class Mul:
    a: object
    b: object


two_times = Mul(Lit(2), Add(Lit(3), Lit(4)))


# ANCHOR: procedural
# Python (procedural style: each function walks the tree itself)
# Python（手続き型スタイル：関数ごとに、自分で木をたどる）
def evaluate(e):
    match e:
        case Lit(n):
            return n
        case Add(a, b):
            return evaluate(a) + evaluate(b)
        case Mul(a, b):
            return evaluate(a) * evaluate(b)


def show(e):
    match e:
        case Lit(n):
            return str(n)
        case Add(a, b):
            return f"({show(a)} + {show(b)})"
        case Mul(a, b):
            return f"{show(a)} * {show(b)}"
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: one fold walks the tree; pass what to do)
# Python（関数型スタイル：木をたどるのは1つの fold。やることを渡す）
def fold_expr(lit, add, mul, e):
    match e:
        case Lit(n):
            return lit(n)
        case Add(a, b):
            return add(fold_expr(lit, add, mul, a),
                       fold_expr(lit, add, mul, b))
        case Mul(a, b):
            return mul(fold_expr(lit, add, mul, a),
                       fold_expr(lit, add, mul, b))


value = fold_expr(lambda n: n, lambda a, b: a + b, lambda a, b: a * b,
                  two_times)
# ANCHOR_END: functional

assert evaluate(two_times) == value == 14
assert show(two_times) == "2 * (3 + 4)"
print("ok")
