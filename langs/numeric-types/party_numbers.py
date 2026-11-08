# The numbers of the chapter "Number types and conversions", in Python.
# 「数の型と変換」の章の数を、Python で
import ctypes
import math

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
levels = [12, 15, 9, 20]
total = 0
for level in levels:
    total += level
# / always gives a float / / はいつも float を返す
average = total / len(levels)        # 14.0
# ANCHOR_END: procedural
assert average == 14.0

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
average = sum(levels) / len(levels)  # 14.0
damage = math.floor(33 * 1.5)        # 49
# ANCHOR_END: functional
assert average == 14.0 and damage == 49

# ANCHOR: narrow
# Python
# int never overflows, but a C-sized integer wraps
# int はあふれないが、C の大きさの整数は一周する
wrapped = ctypes.c_int8(300).value   # 44
# ANCHOR_END: narrow
assert wrapped == 44

# The book also says: some conversions refuse instead of wrapping.
# 本文の説明：一周させずに、断る変換もある
import array
for refuse in (lambda: array.array("b", [300]),
               lambda: (300).to_bytes(1, signed=True)):
    try:
        refuse()
        raise AssertionError("expected OverflowError")
    except OverflowError:
        pass
assert 100 * 2 ** 60 == 115292150460684697600
print("ok")
