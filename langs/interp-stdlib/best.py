# The best score, in Python: a loop first, then the built-in functions.
# いちばんよい点を、Python で：まずループ、次に組み込みの関数

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def best(scores):
    top = None
    for s in scores:
        if top is None or s > top:
            top = s
    return top          # None when there is no score
                        # 点数がなければ None
# ANCHOR_END: procedural

assert best([95, 88, 79]) == 95
assert best([]) is None

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
totals = {"Python": 247, "TS": 159, "Rust": 262}
top = max(totals.values())            # 262
everyone = sum(totals.values())       # 668
low = any(t < 200 for t in totals.values())   # True
nothing = max([], default=None)       # None
# A dict gives its KEYS: the biggest name, not the biggest total.
# dict が渡すのはキー：いちばん大きい名前で、合計ではない
first = max(totals)                   # 'TS'
# ANCHOR_END: functional

assert top == 262
assert everyone == 668
assert low is True
assert nothing is None
assert first == "TS"
# Without default, max of nothing stops the program.
# default なしだと、空の max はプログラムを止める
try:
    max([])
    raise AssertionError("max([]) did not raise")
except ValueError as e:
    assert "empty" in str(e), e
print("ok best.py")
