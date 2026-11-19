# Run errors.pl with Perl 5.44 in Docker (the official image), and check
# what it prints. Without Docker, it is skipped with a message.
# errors.pl を Docker の Perl 5.44（公式のイメージ）で動かし、出力を
# 確かめる。Docker がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"
want = """Invalid [] range "z-a" in regex; marked by <-- HERE in m/[z-a <-- HERE ]/
Unmatched ( in regex; marked by <-- HERE in m/( <-- HERE ab/
Unmatched ) in regex; marked by <-- HERE in m/ab) <-- HERE /
Nested quantifiers in regex; marked by <-- HERE in m/a** <-- HERE /
Quantifier {n,m} with n > m can't match in regex; marked by <-- HERE in m/a{5,2} <-- HERE /
no match
"""

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "errors.pl")),
        capture_output=True, text=True, check=True).stdout
    assert out == want, out
    print(out, end="")
