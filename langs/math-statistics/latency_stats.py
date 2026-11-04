# The statistics of the chapter "Basic statistics", in Python.
# 「統計の基本」の章の統計を、Python で
import math
import statistics

import numpy as np

gold = [30, 45, 50, 52, 60, 65, 70, 80, 90, 120, 100000]

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
total = 0
for g in gold:
    total += g
average = total / len(gold)              # 9151.09...
spread = 0
for g in gold:
    spread += (g - average) ** 2
deviation = math.sqrt(spread / len(gold))  # 28728.95...
# ANCHOR_END: procedural
assert abs(average - 9151.090909090909) < 1e-9
assert abs(deviation - 28728.95694861496) < 1e-6

# ANCHOR: functional
# Python (functional style: the statistics module)
# Python（関数型スタイル：statistics モジュール）
middle = statistics.median(gold)          # 65
spread = statistics.pstdev(gold)          # the same as above / 上と同じ
times = list(range(1, 101))               # 1, 2, ..., 100 ms
p90_nearest = sorted(times)[math.ceil(0.90 * len(times)) - 1]   # 90
p90_quantiles = statistics.quantiles(times, n=10)[-1]           # 90.9
p90_numpy = float(np.percentile(times, 90))                     # 90.1
# ANCHOR_END: functional
assert middle == 65 and abs(spread - deviation) < 1e-6
assert p90_nearest == 90
assert abs(p90_quantiles - 90.9) < 1e-9, p90_quantiles
assert abs(p90_numpy - 90.1) < 1e-9, p90_numpy
print("ok")
