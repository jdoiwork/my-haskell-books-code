# A small stopwatch shared by the Python benchmarks: warm up once, then
# the median of several runs, like Bench.Timer in Haskell.
# Python のベンチマークで使う小さなストップウォッチ。1回温めてから、
# 何回か動かした中央値（Haskell の Bench.Timer と同じ）
import os
import time

# BOOK_BENCH=1 measures for the book. Otherwise (check-langs.sh) the
# sizes are small and every program only checks its answers.
# BOOK_BENCH=1 のときは、本のために測る。それ以外（check-langs.sh）では、
# 大きさを小さくして、答えを確かめるだけ
FULL = os.environ.get("BOOK_BENCH") == "1"


# ANCHOR: median
def median_ms(reps, f, *args):
    f(*args)  # warm up / 温める
    times = []
    for _ in range(reps):
        t0 = time.perf_counter()
        f(*args)
        times.append(time.perf_counter() - t0)
    times.sort()
    return times[reps // 2] * 1000
# ANCHOR_END: median


def report(task, n, how, ms):
    print(f"{task:<8} n={n:<5} {how:<14} {ms:10.2f} ms")
