# The comparison code in the introduction (even squares).
# 「はじめに」の比較のコード（偶数の2乗）
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
even_squares = []
for x in range(1, 11):
    if x % 2 == 0:
        even_squares.append(x * x)
# ANCHOR_END: procedural
assert even_squares == [4, 16, 36, 64, 100]
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
even_squares = [x * x for x in range(1, 11) if x % 2 == 0]
# ANCHOR_END: functional
assert even_squares == [4, 16, 36, 64, 100]
