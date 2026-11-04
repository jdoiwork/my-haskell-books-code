# The Python code in the NumPy chapter.
# NumPy の章の Python のコード

# ANCHOR: procedural
# Python (procedural style: add a row to every row with loops)
# Python（手続き型スタイル：ループで、どの行にも同じ行を足す）
a = [[1, 2, 3], [4, 5, 6]]
v = [10, 20, 30]
result = []
for row in a:
    new_row = []
    for x, y in zip(row, v):
        new_row.append(x + y)
    result.append(new_row)
print(result)        # [[11, 22, 33], [14, 25, 36]]
# ANCHOR_END: procedural

assert result == [[11, 22, 33], [14, 25, 36]]

# ANCHOR: numpy
# Python (NumPy: the same, with broadcasting)
# Python（NumPy：同じことを、ブロードキャストで）
import numpy as np

a = np.array([[1., 2., 3.], [4., 5., 6.]])
print(a.shape)                                 # (2, 3)
print(a + np.array([10., 20., 30.]))           # adds to every row
print(a * 2)
print(a + np.array([[100.], [200.]]))          # adds to every column
print(np.dot(a, a.T))                          # [[14, 32], [32, 77]]
print(a.sum(axis=0), a.sum(axis=1), a.mean())  # [5 7 9] [6 15] 3.5
# ANCHOR_END: numpy

assert a.shape == (2, 3)
assert (a + np.array([10., 20., 30.])).tolist() == [[11, 22, 33],
                                                   [14, 25, 36]]
assert (a + np.array([[100.], [200.]])).tolist() == [[101, 102, 103],
                                                    [204, 205, 206]]
assert np.dot(a, a.T).tolist() == [[14, 32], [32, 77]]
assert a.sum(axis=0).tolist() == [5, 7, 9]
assert a.sum(axis=1).tolist() == [6, 15]
assert a.mean() == 3.5

# ANCHOR: error
# Python (NumPy: shapes that do not fit stop at run time)
# Python（NumPy：形が合わないと、動かしたときに止まる）
try:
    a + np.array([1., 2.])
except ValueError as e:
    print(e)
# operands could not be broadcast together with shapes (2,3) (2,)
# ANCHOR_END: error

try:
    a + np.array([1., 2.])
    raise AssertionError("should fail")
except ValueError as e:
    assert "could not be broadcast" in str(e)
