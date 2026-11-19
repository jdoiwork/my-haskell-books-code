# Run linear.pl with Perl 5.44 in Docker (the official image), and check
# what it prints: nothing is found, the pattern without \1 is quick, and
# the one with \1 takes longer and longer. Without Docker, it is skipped
# with a message.
# linear.pl を Docker の Perl 5.44（公式のイメージ）で動かし、出力を
# 確かめる：どれも見つからない、\1 のないものは一瞬、\1 のあるものは
# だんだん長くかかる。Docker がなければ、知らせて飛ばす
import os
import re
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "linear.pl")),
        capture_output=True, text=True, check=True).stdout
    print(out, end="")
    rows = [re.match(r"(\d) (\S+)\s+(not found), ([\d.]+)s$", line)
            for line in out.splitlines()]
    assert all(rows) and len(rows) == 6, out
    plain = [float(r[4]) for r in rows if not r[2].endswith(r"\1$)")]
    backref = [float(r[4]) for r in rows if r[2].endswith(r"\1$)")]
    assert max(plain) < 0.05, out
    assert backref[2] > 4 * backref[0], out
