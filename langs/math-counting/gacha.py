# Counting and chance of the chapter "Counting and probability", in Python.
# 「数え上げと確率」の章の数え上げと確率を、Python で
import itertools
import math
import random
from fractions import Fraction

# ANCHOR: procedural
# Python (procedural style: try it many times)
# Python（手続き型スタイル：何度も試す）
random.seed(42)
total = 0
for trial in range(10000):
    pulls = 1
    while random.random() >= 0.01:
        pulls += 1
    total += pulls
average = total / 10000            # close to 100 / 100 に近い
# ANCHOR_END: procedural
assert 95 < average < 105, average

# ANCHOR: functional
# Python (functional style: count exactly)
# Python（関数型スタイル：ちょうど数える）
heroes = ["Lambda", "Python", "Rust", "TS"]
teams = list(itertools.combinations(heroes, 2))   # 6 teams
ways = math.comb(52, 5)                            # 2598960
dice = [(a, b) for a in range(1, 7) for b in range(1, 7)]
seven = Fraction(sum(1 for a, b in dice if a + b == 7), len(dice))
no_birthday_match = math.prod((365 - k) / 365 for k in range(23))
# ANCHOR_END: functional
assert len(teams) == 6 and ways == 2598960
assert seven == Fraction(1, 6)
assert abs((1 - no_birthday_match) - 0.5072972343239857) < 1e-12
print("ok")
