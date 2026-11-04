# Fast power with its loop invariant checked by assert, in Python.
# 速いべき乗を、ループ不変条件を assert で確かめながら、Python で

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def power(x, n):
    acc, b, e = 1, x, n
    while e > 0:
        assert acc * b ** e == x ** n   # the invariant / 不変条件
        if e % 2 == 0:
            b, e = b * b, e // 2
        else:
            acc, e = acc * b, e - 1
    return acc
# ANCHOR_END: procedural

assert power(2, 10) == 1024
assert power(3, 5) == 243
assert all(power(x, n) == x ** n for x in range(-5, 6) for n in range(41))
print("ok")
