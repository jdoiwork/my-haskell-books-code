# Run captures.pl with Perl 5.44 in Docker (the official image), and check
# what it prints. Without Docker, it is skipped with a message.
# captures.pl を Docker の Perl 5.44（公式のイメージ）で動かし、出力を
# 確かめる。Docker がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"
want = "19/11/2026 8..10\nundef\n<>,<a>,<>\n"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "captures.pl")),
        capture_output=True, text=True, check=True).stdout
    assert out == want, out
    print(out, end="")
