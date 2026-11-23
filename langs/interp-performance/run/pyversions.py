# Python: the same small work, timed inside Python (no start-up), to
# compare the versions of Python. Written to run on Python 3.8 too (no
# match, no new syntax).
#   python3 pyversions.py LOG
# Python：同じ小さな仕事を、Python の中で測る（起動の時間は入らない）。
# Python の版どうしをくらべるため。Python 3.8 でも動くように書いた
# （match も、新しい書き方も使わない）
import re
import sys
import time
from collections import Counter


# ANCHOR: work
# Python
def loop():
    total, n = 0, 1
    while n <= 3_000_000:
        total, n = total + n, n + 1
    return total


def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


def sort():
    xs = [i * 7919 % 100003 for i in range(1, 1_000_001)]
    return sorted(xs)[:5]


line = re.compile(
    r'^(?P<ip>\S+) .*" (?P<status>\d{3}) \d+ rt=(?P<rt>[\d.]+)$')


def slow(path):
    counts = Counter()
    with open(path, encoding="utf-8") as f:
        for text in f:
            m = line.match(text.rstrip("\n"))
            if m and int(m["status"]) >= 500 and float(m["rt"]) > 0.5:
                counts[m["ip"]] += 1
    return counts.most_common(1)
# ANCHOR_END: work


# ⏱️ The shortest of five runs, in seconds.
# 5回のうち、いちばん短い時間（秒）
def best(work, *args):
    times = []
    for _ in range(5):
        start = time.perf_counter()
        work(*args)
        times.append(time.perf_counter() - start)
    return min(times)


version = sys.version.split()[0]
name = sys.implementation.name
log = sys.argv[1]
print(f"{name} {version}: loop {best(loop):.3f}  fib(30) {best(fib, 30):.3f}"
      f"  sorted {best(sort):.3f}  regex {best(slow, log):.3f}")
