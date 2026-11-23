# Python: the programs of examples/perf of the chapter, written by hand
# as Python is usually written (not turned from Lam).
#   python3 perf.py loop | fib | range | sort | deep | count FILE | slow FILE
# Python：章の examples/perf のプログラムを、Python でふつうに書いたもの
# （Lam から変換したものではない）
import re
import sys
from collections import Counter


# ANCHOR: loop
# Python
# A million steps: Python has no tail calls, so a while loop.
# 百万歩。Python は末尾呼び出しをしないので、while のループで
def loop():
    total, n = 0, 1
    while n <= 1_000_000:
        total, n = total + n, n + 1
    return total
# ANCHOR_END: loop


def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


# ANCHOR: deep
# Python
def deep():
    print(loop())
    xs = list(range(1, 200_001))
    print(sum(x * 2 for x in xs))
    print(len(sorted(reversed(xs))))
# ANCHOR_END: deep


def count(path):
    n = 0
    with open(path, encoding="utf-8") as f:
        for _ in f:
            n += 1
    return n


# ANCHOR: slow
# Python
line = re.compile(
    r'^(?P<ip>\S+) .*" (?P<status>\d{3}) \d+ rt=(?P<rt>[\d.]+)$')


def slow(path):
    counts = Counter()
    with open(path, encoding="utf-8") as f:
        for text in f:
            m = line.match(text.rstrip("\n"))
            if m and int(m["status"]) >= 500 and float(m["rt"]) > 0.5:
                counts[m["ip"]] += 1
    # The biggest first; the same counts by IP, as Lam's Map gives them.
    # 大きい順。同じ数は、Lam の Map と同じく IP の順
    top = sorted(sorted(counts.items()), key=lambda kv: -kv[1])[:5]
    for ip, n in top:
        print(f"{n:>5}  {ip}")
# ANCHOR_END: slow


def main(args):
    match args:
        case ["loop"]:
            print(loop())
        case ["fib"]:
            print(fib(27))
        case ["range"]:
            print(sum(range(1, 1_000_001)))
        case ["sort"]:
            xs = [i * 7919 % 100003 for i in range(1, 100_001)]
            ys = sorted(xs)
            print(ys[:5])
            print(len(ys))
        case ["deep"]:
            deep()
        case ["count", path]:
            print(count(path))
        case ["slow", path]:
            slow(path)
        case _:
            sys.exit("usage: perf.py loop|fib|range|sort|deep|count F|slow F")


main(sys.argv[1:])
