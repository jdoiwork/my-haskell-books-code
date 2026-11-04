# Vectors and matrices of the chapter "Vectors and matrices", in Python.
# 「ベクトルと行列」の章のベクトルと行列を、Python で
import math

import numpy as np

slime = [2, 2, 3, 1]
dragon = [30, 25, 15, 20]

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
total = 0
for a, b in zip(slime, dragon):
    total += a * b                       # the dot product / 内積
length_s = math.sqrt(sum(a * a for a in slime))
length_d = math.sqrt(sum(b * b for b in dragon))
similarity = total / (length_s * length_d)   # 0.889...
# ANCHOR_END: procedural
assert abs(similarity - 0.8895749936068605) < 1e-12

# ANCHOR: functional
# Python (NumPy: vectors and matrices are arrays)
# Python（NumPy：ベクトルも行列も配列）
s, d = np.array(slime), np.array(dragon)
similarity = s @ d / (np.linalg.norm(s) * np.linalg.norm(d))
rotate90 = np.array([[0, -1],
                     [1,  0]])
stretch_x = np.array([[2, 0],
                      [0, 1]])
point = np.array([3, 1])
turned_then_stretched = stretch_x @ rotate90 @ point    # [-2, 3]
stretched_then_turned = rotate90 @ stretch_x @ point    # [-1, 6]
# ANCHOR_END: functional
assert abs(similarity - 0.8895749936068605) < 1e-12
assert list(turned_then_stretched) == [-2, 3]
assert list(stretched_then_turned) == [-1, 6]
print("ok")
