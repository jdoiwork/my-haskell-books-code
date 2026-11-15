# The Python code in the chapter on strings, tuples and lists.
# 文字列・タプル・リストの章の Python のコード

# ANCHOR: procedural
# Python (procedural style): and as a keyword, and as a function
# Python（手続き型スタイル）：キーワードの and と、関数の and
def my_and(a, b):
    if a:
        return b
    return False

print(False and 1 / 0 == 0)       # False: the right side is not computed
try:
    print(my_and(False, 1 / 0 == 0))
except ZeroDivisionError:
    print("ZeroDivisionError")     # the argument is computed first
# ANCHOR_END: procedural

# ANCHOR: functional
# Python (functional style): pass the right side as a function
# Python（関数型スタイル）：右側を、関数に包んで渡す
def lazy_and(a, b):
    return b() if a else False

print(lazy_and(False, lambda: 1 / 0 == 0))   # False
# ANCHOR_END: functional

assert (False and 1 / 0 == 0) is False
try:
    my_and(False, 1 / 0 == 0)
    raise AssertionError("my_and should have failed")
except ZeroDivisionError:
    pass
assert lazy_and(False, lambda: 1 / 0 == 0) is False
assert lazy_and(True, lambda: 2 > 1) is True
