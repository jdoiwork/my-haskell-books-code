# The chapter on measuring: the same jobs as MiniRegex's benchmark, with
# Python's re. Run as: python3 bench.py LINES ROUNDS
# Prints one line for each job: the job, the count, the checksum, and
# the middle time of the rounds in microseconds.
# 測って比べる章：MiniRegex のベンチマークと同じ仕事を、Python の re で。
# python3 bench.py 行の数 回数 で動かす。仕事ごとに1行：仕事、件数、
# 確かめの数、回のまん中の時間（マイクロ秒）
import re
import statistics
import sys
import time

# ANCHOR: book
NAMES = ["Mira", "Ruby", "Lam", "GHC", "Perl", "Rust", "Python", "Go"]
ITEMS = ["herbs", "potions", "swords", "maps", "scrolls"]


# 📒 The guild's book of sales: n lines, the same in every language.
# ギルドの売り上げのログ：n 行。どの言語でも同じもの
def guild_book(n):
    lines = []
    for i in range(n):
        seller = NAMES[i % 8]
        line = (f"2026-11-{i % 28 + 1:02} {i % 24:02}:{i * 7 % 60:02} "
                f"{seller} sold {i % 9 + 1} {ITEMS[i % 5]} "
                f"to {NAMES[(3 * i + 1) % 8]} for {i * 37 % 1000} gold")
        if i % 7 == 0:
            line += f" <{seller.lower()}@guild.example>"
        lines.append(line + "\n")
    return "".join(lines)
# ANCHOR_END: book


# ANCHOR: jobs
JOBS = [
    ("dates", r"\d{4}-\d{2}-\d{2}"),
    ("mail", r"[\w.+-]+@[\w-]+\.[\w.-]+"),
    ("names", r"Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"),
    ("caseless", r"(?i)perl"),
    ("sales", r"(\w+) sold (\d+) (\w+)"),
]


# 🔢 Count the matches; the checksum adds up group 2 where there is one.
# 一致を数える。確かめの数は、2番のグループがあれば、その数の合計
def count(regex, text):
    n, total = 0, 0
    for m in regex.finditer(text):
        n += 1
        if regex.groups >= 2:
            total += int(m.group(2))
    return n, total
# ANCHOR_END: jobs


# 🕰️ The middle time of the rounds, in microseconds.
# 回の、まん中の時間（マイクロ秒）
def clock(job, rounds):
    times = []
    answer = None
    for _ in range(rounds):
        start = time.perf_counter()
        answer = job()
        times.append((time.perf_counter() - start) * 1_000_000)
    return answer, statistics.median(times)


def main():
    lines, rounds = int(sys.argv[1]), int(sys.argv[2])
    text = guild_book(lines)
    for name, pattern in JOBS:
        regex = re.compile(pattern)
        (n, total), us = clock(lambda: count(regex, text), rounds)
        print(f"{name} {n} {total} {us:.2f}")
    # the name field: one search, eight names and a ! by mistake
    # 名前の欄：探すのは1回。名前が8個と、まちがえて入った !
    field = " ".join((NAMES[:5] * 2)[:8]) + "!"
    regex = re.compile(r"^(\w+\s?)+$")
    found, us = clock(lambda: regex.search(field), rounds)
    print(f"field {0 if found is None else 1} 0 {us:.2f}")


if __name__ == "__main__":
    main()
