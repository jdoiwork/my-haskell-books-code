# Merge sort, for the chapter "Divide and conquer".
# マージソート。「分割統治」の章
from heapq import merge


# ANCHOR: procedural
# Python (procedural style: merge with two indexes)
# Python（手続き型スタイル：2つの位置を進めながらまとめる）
def msort(xs):
    if len(xs) <= 1:
        return xs
    mid = len(xs) // 2
    left, right = msort(xs[:mid]), msort(xs[mid:])
    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    return result + left[i:] + right[j:]
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: heapq.merge merges sorted iterables lazily)
# Python（関数型スタイル：heapq.merge が、並んだものを遅延でまとめる）
def msort_f(xs):
    if len(xs) <= 1:
        return xs
    mid = len(xs) // 2
    return list(merge(msort_f(xs[:mid]), msort_f(xs[mid:])))
# ANCHOR_END: functional


data = [5, 2, 8, 1, 9]
assert msort(data) == msort_f(data) == [1, 2, 5, 8, 9]
for k in range(100):
    xs = [(i * 7919 + k) % 37 for i in range(k)]
    assert msort(xs) == msort_f(xs) == sorted(xs)
print("ok")
