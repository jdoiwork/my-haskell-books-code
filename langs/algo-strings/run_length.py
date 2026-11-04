# Run-length encoding, for the chapter "String algorithms".
# ランレングス符号。「文字列のアルゴリズム」の章
from itertools import groupby


# ANCHOR: procedural
# Python (procedural style: count the current run in a loop)
# Python（手続き型スタイル：ループで、今の並びの長さを数える）
def run_length(s):
    runs = []
    for c in s:
        if runs and runs[-1][0] == c:
            runs[-1] = (c, runs[-1][1] + 1)
        else:
            runs.append((c, 1))
    return runs
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: itertools.groupby, like Haskell's group)
# Python（関数型スタイル：itertools.groupby。Haskell の group と同じ）
def run_length_f(s):
    return [(c, len(list(run))) for c, run in groupby(s)]
# ANCHOR_END: functional


expected = [("a", 3), ("b", 1), ("c", 2), ("d", 2)]
assert run_length("aaabccdd") == run_length_f("aaabccdd") == expected
assert run_length("") == run_length_f("") == []
print("ok")
