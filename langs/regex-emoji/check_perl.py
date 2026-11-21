# Run emoji.pl with Perl 5.44 in Docker (the official image), and check
# what it prints. Without Docker, it is skipped with a message.
# emoji.pl を Docker の Perl 5.44（公式のイメージ）で動かし、出力を
# 確かめる。Docker がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"
want = "5 3\n7 # 1\n"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "emoji.pl")),
        capture_output=True, text=True, check=True).stdout
    assert out == want, out
    print(out, end="")
