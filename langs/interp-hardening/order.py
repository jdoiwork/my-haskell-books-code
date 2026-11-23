# Python: the names in a function are looked up when it runs, so the
# order of the definitions does not matter, as long as they are all
# defined before the first call.
# Python：関数の中の名前は動くときに引くので、最初に呼ぶまでに全部
# 定義してあれば、定義の順はどうでもよい

# ANCHOR: order
# Python (procedural style)
# Python（手続き型スタイル）
def main():
    print(is_even(10), is_odd(7))
    print(greet("Lam"))


def is_even(n):
    if n == 0:
        return True
    return is_odd(n - 1)


def is_odd(n):
    if n == 0:
        return False
    return is_even(n - 1)


def greet(name):
    return "Hello, " + name + SUFFIX


SUFFIX = "!"

main()
# ANCHOR_END: order

# ANCHOR: tooEarly
# Called before it is defined, at the top of the file: a NameError.
# ファイルの上のほうで、定義より前に呼ぶと NameError
try:
    exec("print(later())\ndef later():\n    return 1\n", {})
    raise AssertionError("no NameError")
except NameError as e:
    assert str(e) == "name 'later' is not defined", e
# ANCHOR_END: tooEarly

assert is_even(10) and is_odd(7)
assert greet("Lam") == "Hello, Lam!"
