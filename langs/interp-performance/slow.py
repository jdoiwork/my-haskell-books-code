# Python: the requests that failed and were slow, counted by IP (the
# same as examples/perf/slow.lam)
# Python：失敗して遅かった頼みを、IP ごとに数える
# （examples/perf/slow.lam と同じ）
import re
import sys
from collections import Counter

# ANCHOR: slow
# Python
line = re.compile(
    r'^(?P<ip>\S+) .*" (?P<status>\d{3}) \d+ rt=(?P<rt>[\d.]+)$')


def count(path):
    counts = Counter()
    with open(path, encoding="utf-8") as f:
        for text in f:
            m = line.match(text.rstrip("\n"))
            if m and int(m["status"]) >= 500 and float(m["rt"]) > 0.5:
                counts[m["ip"]] += 1
    return counts
# ANCHOR_END: slow


def top(counts):
    # The biggest first; the same counts in the order they came.
    # 大きい順。同じ数は、来た順
    pairs = sorted(counts.items(), key=lambda kv: kv[0])
    return sorted(pairs, key=lambda kv: -kv[1])[:5]


if len(sys.argv) > 1:
    for ip, n in top(count(sys.argv[1])):
        print(f"{n:>5}  {ip}")
else:
    # A small check: three lines, two of them slow failures of one IP.
    # 小さな確かめ：3行のうち2行が、同じ IP の遅い失敗
    import os
    import tempfile
    rows = [
        '10.0.1.2 - - [x] "GET / HTTP/1.1" 503 10 rt=0.900',
        '10.0.1.2 - - [x] "GET / HTTP/1.1" 500 10 rt=0.600',
        '10.0.1.3 - - [x] "GET / HTTP/1.1" 200 10 rt=0.900',
    ]
    with tempfile.NamedTemporaryFile("w", delete=False) as f:
        f.write("\n".join(rows) + "\n")
    try:
        assert top(count(f.name)) == [("10.0.1.2", 2)]
    finally:
        os.remove(f.name)
