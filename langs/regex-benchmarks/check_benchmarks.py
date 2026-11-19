# The chapter on measuring (MiniRegex): run the same jobs in Python, C
# (PCRE2, without and with the JIT), Rust (the regex crate) and Perl
# 5.44 (in Docker), and check that they all count the same. check-langs
# runs it small (2000 lines, 3 rounds); with --full, 20000 lines and 11
# rounds, and a table of the times (the one in the chapter).
# 測って比べる章（MiniRegex）：同じ仕事を、Python・C（PCRE2 の JIT
# なし・あり）・Rust（regex クレート）・Perl 5.44（Docker）で動かし、
# どれも同じ数を数えるかを確かめる。check-langs は小さく動かす（2000 行、
# 3回）。--full なら 2万行・11回で、時間の表も出す（章に載せた表）
import os
import subprocess
import sys

here = os.path.dirname(os.path.abspath(__file__))
run_dir = os.path.join(here, "run")
cargo = os.path.join(here, "..", "cargo")
full = "--full" in sys.argv
lines, rounds = ("20000", "11") if full else ("2000", "3")
tmp = os.environ.get("TMPDIR", "/tmp")


def table(out, engine=None):
    rows = {}
    for line in out.splitlines():
        parts = line.split()
        if engine is None:
            parts = ["-"] + parts
        e, job, n, total, us = parts
        rows[(e, job)] = (int(n), int(total), float(us))
    return rows


results = {}

# Python
out = subprocess.run(
    [sys.executable, os.path.join(run_dir, "bench.py"), lines, rounds],
    capture_output=True, text=True, check=True).stdout
results.update({("python", j): v for (_, j), v in table(out).items()})

# C (PCRE2 of Homebrew)
exe = os.path.join(tmp, "regex_benchmarks_c")
subprocess.run(
    ["/usr/bin/clang", "-O2", "-Wall", "-Werror",
     "-I/opt/homebrew/opt/pcre2/include", "-L/opt/homebrew/opt/pcre2/lib",
     "-o", exe, os.path.join(run_dir, "bench.c"), "-lpcre2-8"], check=True)
out = subprocess.run([exe, lines, rounds], capture_output=True, text=True,
                     check=True).stdout
results.update(table(out, engine=True))

# Rust (regex, release build)
subprocess.run(["cargo", "build", "-q", "--release", "--bin",
                "regex_benchmarks"], cwd=cargo, check=True)
out = subprocess.run(
    [os.path.join(cargo, "target", "release", "regex_benchmarks"),
     lines, rounds], capture_output=True, text=True, check=True).stdout
results.update({("rust", j): v for (_, j), v in table(out).items()})

# Perl 5.44 (Docker)
if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i",
         "perl:5.44-slim", "perl", "-", lines, rounds],
        stdin=open(os.path.join(run_dir, "bench.pl")),
        capture_output=True, text=True, check=True).stdout
    results.update({("perl", j): v for (_, j), v in table(out).items()})

# The counts: the same in every language (and the same as MiniRegex's
# test). PCRE2 gives up on the name field at its match limit (-1).
# 件数：どの言語でも同じ（MiniRegex のテストとも同じ）。名前の欄では、
# PCRE2 は手数の上限であきらめる（-1）
n = int(lines)
expected = {
    "dates": (n, 0),
    "mail": ((n + 6) // 7, 0),
    "names": (2 * n, 0),
    "caseless": ({2000: 536, 20000: 5357}[n], 0),
    "sales": (n, {2000: 9993, 20000: 99993}[n]),
}
engines = sorted({e for e, _ in results})
for e in engines:
    for job, want in expected.items():
        got = results[(e, job)][:2]
        assert got == want, (e, job, got, want)
    field = results[(e, "field")][0]
    assert field == (-1 if e.startswith("pcre2") else 0), (e, field)

# The table, in milliseconds (the name field in microseconds).
# 表。ミリ秒で（名前の欄はマイクロ秒）
jobs = list(expected) + ["field"]
print(f"{'':10}" + "".join(f"{j:>10}" for j in jobs))
for e in engines:
    cells = []
    for job in jobs:
        us = results[(e, job)][2]
        cells.append(f"{us:10.2f}" if job == "field" else f"{us / 1000:10.2f}")
    print(f"{e:10}" + "".join(cells))
