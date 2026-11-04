# The Python code in the laziness chapter.
# 遅延評価の章の Python のコード
from itertools import count, islice

# ANCHOR: lazy
# Python (a generator: values are made only when asked for)
# Python（ジェネレーター：求められたときだけ値を作る）
def doubled():
    for n in count(1):
        print(f"making {n * 2}")
        yield n * 2

first3 = list(islice(doubled(), 3))
# ANCHOR_END: lazy
assert first3 == [2, 4, 6]

# ANCHOR: strings
# Python (len counts characters)
# Python（len は文字の数）
assert len("冒険") == 2
assert len("😀") == 1
assert len("😀".encode("utf-8")) == 4
# ANCHOR_END: strings
print("ok")
