# Python: count the lines of a log, one line at a time (the same as
# examples/perf/count.lam)
# Python：ログの行を、1行ずつ数える（examples/perf/count.lam と同じ）
import sys


# ANCHOR: count
# Python
def count(path):
    n = 0
    with open(path, encoding="utf-8") as f:
        for _ in f:
            n += 1
    return n
# ANCHOR_END: count


if len(sys.argv) > 1:
    print(count(sys.argv[1]))
else:
    import os
    import tempfile
    with tempfile.NamedTemporaryFile("w", delete=False) as f:
        f.write("a\nb\nc\n")
    try:
        assert count(f.name) == 3
    finally:
        os.remove(f.name)
