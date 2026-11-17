# The Python code in the chapter on floats.
# 小数の章の Python のコード
import math

scores = [82, 75, 91, 64]

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def average(xs):
    total = 0
    for x in xs:
        total += x
    return total / len(xs)      # / always gives a float: 78.0
# ANCHOR_END: procedural

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def average2(xs):
    return sum(xs) / len(xs)

def whole_average(xs):
    return sum(xs) // len(xs)   # // rounds down: -7 // 2 is -4
# ANCHOR_END: functional

# ANCHOR: surprises
# Python (a few surprises)
# Python（ちょっとした驚き）
halves = (round(2.5), round(3.5))      # (2, 4): a half goes to even
nan_same = math.nan == math.nan        # False
try:
    1.0 / 0.0                          # not inf: an error
except ZeroDivisionError as e:
    zero = str(e)                      # 'division by zero'
# ANCHOR_END: surprises

assert average(scores) == 78.0 and average2(scores) == 78.0
assert whole_average([7, 8]) == 7 and -7 // 2 == -4
assert halves == (2, 4)
assert nan_same is False
assert zero == "division by zero"
assert 0.1 + 0.2 == 0.30000000000000004
print(average(scores), whole_average([7, 8]), halves, zero)
