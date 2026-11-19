# Run hide.pl with Perl 5.44 in Docker (the official image), and check
# what it prints, and the warning of the typo. Without Docker, it is
# skipped with a message.
# hide.pl を Docker の Perl 5.44（公式のイメージ）で動かし、出力と、
# 書きまちがいの警告を確かめる。Docker がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"
want = ("call ***-****-5432 or ***-****-5678 now\n"
        "2026-11-20 ERROR\n"
        "call ***-****- or ***-****- now\n")

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    done = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "hide.pl")),
        capture_output=True, text=True, check=True)
    assert done.stdout == want, done.stdout
    assert "Use of uninitialized value" in done.stderr, done.stderr
    print(done.stdout, end="")
    print(done.stderr.splitlines()[0])
