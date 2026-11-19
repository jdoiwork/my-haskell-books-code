# Run repeat.pl with Perl 5.44 in Docker (the official image), and keep
# only the final programs from what `use re 'debug'` prints. Without
# Docker, it is skipped with a message.
# repeat.pl を Docker の Perl 5.44（公式のイメージ）で動かし、
# use re 'debug' が出すもののうち、最後のプログラムだけを取り出して
# 確かめる。Docker がなければ、知らせて飛ばす
import os
import re
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"


def final_programs(debug):
    programs, current, inside = [], None, False
    for line in debug.splitlines():
        if line.startswith("Compiling REx"):
            current = [line]
            programs.append(current)
        elif line.startswith("Final program:"):
            inside = True
        elif inside and re.match(r"\s*\d+:", line):
            current.append(line.rstrip())
        elif not line.startswith(" "):
            inside = False
    return programs


if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    debug = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "repeat.pl")),
        capture_output=True, text=True, check=True).stderr
    programs = final_programs(debug)
    for p in programs:
        print("\n".join(p))
    names = [[line.split(":", 1)[1].split()[0] for line in p[1:]]
             for p in programs]
    assert names[0] == ["STAR", "EXACT", "EXACT", "END"], names[0]
    assert names[1][0].startswith("CURLYM"), names[1]
    assert names[2][1].startswith("CURLYX"), names[2]
    assert "PLUS" in names[2] and "WHILEM[1/1]" in names[2], names[2]
    assert names[3][0].startswith("CURLYX"), names[3]
