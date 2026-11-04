# The Python code in the parser chapter.
# パーサーの章の Python のコード
import re

# ANCHOR: descent
# Python (a hand-written recursive descent parser for + - * /)
# Python（+ - * / を読む、手書きの再帰下降パーサー）
def tokenize(text):
    return re.findall(r"\d+|[-+*/()]", text)

def parse(text):
    tokens = tokenize(text)
    pos = 0

    def peek():
        return tokens[pos] if pos < len(tokens) else None

    def take():
        nonlocal pos
        pos += 1
        return tokens[pos - 1]

    def atom():                      # 42 or ( sum )
        if peek() == "(":
            take()
            e = sum_()
            if take() != ")":
                raise SyntaxError("expected )")
            return e
        return ("lit", int(take()))

    def product():                   # atom (* atom)*
        e = atom()
        while peek() in ("*", "/"):
            e = (take(), e, atom())
        return e

    def sum_():                      # product (+ product)*
        e = product()
        while peek() in ("+", "-"):
            e = (take(), e, product())
        return e

    result = sum_()
    if peek() is not None:
        raise SyntaxError(f"unexpected {peek()}")
    return result
# ANCHOR_END: descent

assert parse("1 + 2 * 3") == (
    "+", ("lit", 1), ("*", ("lit", 2), ("lit", 3)))
assert parse("1 - 2 - 3") == ("-", ("-", ("lit", 1), ("lit", 2)), ("lit", 3))
assert parse("(1 + 2) * 3") == ("*", ("+", ("lit", 1), ("lit", 2)), ("lit", 3))
print("ok")
