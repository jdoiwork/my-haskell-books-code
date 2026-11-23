# Check the hand-written programs of run/ (Python, TypeScript and awk):
# each gives the same answer as the Lam program of examples/perf.
# run/ の手で書いたプログラム（Python・TypeScript・awk）を確かめる：
# どれも examples/perf の Lam のプログラムと同じ答え
import os
import subprocess
import sys
import tempfile

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
node = os.environ.get("BOOK_NODE", "node")
python = [sys.executable, os.path.join(run, "perf.py")]
ts = [node, "--no-warnings", os.path.join(run, "perf.ts")]
awk_count = ["awk", "END { print NR }"]
awk_slow = ["awk", "-f", os.path.join(run, "slow.awk")]

# A small log: two slow failures of one IP, one fast failure, one success.
# 小さなログ：同じ IP の遅い失敗が2つ、速い失敗が1つ、成功が1つ
rows = [
    '10.0.1.2 - - [x] "GET / HTTP/1.1" 503 10 rt=0.900',
    '10.0.1.2 - - [x] "GET / HTTP/1.1" 500 10 rt=0.600',
    '10.0.1.3 - - [x] "GET / HTTP/1.1" 200 10 rt=0.900',
    '10.0.1.4 - - [x] "GET / HTTP/1.1" 500 10 rt=0.400',
]
expected = {
    "loop": "500000500000\n",
    "fib": "196418\n",
    "range": "500000500000\n",
    "sort": "[1, 2, 3, 4, 5]\n100000\n",
    "deep": "500000500000\n40000200000\n200000\n",
    "count": "4\n",
    "slow": "    2  10.0.1.2\n",
}


def out(command):
    return subprocess.run(command, capture_output=True, text=True,
                          check=True).stdout


with tempfile.NamedTemporaryFile("w", delete=False) as f:
    f.write("\n".join(rows) + "\n")
try:
    for task, answer in expected.items():
        file = [f.name] if task in ("count", "slow") else []
        for command in (python, ts):
            got = out(command + [task] + file)
            assert got == answer, (command[-1], task, got)
    assert out(awk_count + [f.name]) == expected["count"]
    assert out(awk_slow + [f.name]) == expected["slow"]
finally:
    os.remove(f.name)
