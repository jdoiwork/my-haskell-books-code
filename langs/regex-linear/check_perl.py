# Run linear.pl with Homebrew's Perl 5.44 (on the Mac, where the times
# in the chapter were measured), and check what it prints: nothing is
# found, the pattern without \1 is quick, and the one with \1 takes
# longer and longer. Without Perl 5.44, it is skipped with a message.
# linear.pl を Homebrew の Perl 5.44（章の時間を測った Mac の上）で動かし、
# 出力を確かめる：どれも見つからない、\1 のないものは一瞬、\1 のあるものは
# だんだん長くかかる。Perl 5.44 がなければ、知らせて飛ばす
import os
import re
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
perl = "/opt/homebrew/bin/perl"

version = subprocess.run([perl, "-e", "print $^V"], capture_output=True,
                         text=True).stdout if os.path.exists(perl) else ""
if not version.startswith("v5.44"):
    print("skip: no Perl 5.44 (brew install perl)")
else:
    out = subprocess.run([perl, os.path.join(here, "linear.pl")],
                         capture_output=True, text=True, check=True).stdout
    print(out, end="")
    rows = [re.match(r"(\d) (\S+)\s+(not found), ([\d.]+)s$", line)
            for line in out.splitlines()]
    assert all(rows) and len(rows) == 6, out
    plain = [float(r[4]) for r in rows if not r[2].endswith(r"\1$)")]
    backref = [float(r[4]) for r in rows if r[2].endswith(r"\1$)")]
    assert max(plain) < 0.05, out
    assert backref[2] > 4 * backref[0], out
