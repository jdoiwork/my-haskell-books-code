# Run prefilter.pl with Perl 5.44 in Docker (the official image), and
# check what re::regmust tells. Without Docker, it is skipped with a
# message.
# prefilter.pl を Docker の Perl 5.44（公式のイメージ）で動かし、
# re::regmust が教えるものを確かめる。Docker がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
image = "perl:5.44-slim"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl")
else:
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none", "-i", image,
         "perl", "-"],
        stdin=open(os.path.join(here, "prefilter.pl")),
        capture_output=True, text=True, check=True).stdout
    print(out, end="")
    expected = [
        '(?^:Room (\\d+))            anchored "Room "  floating -',
        '(?^:(a+)*b)                anchored -        floating "b"',
        '(?^:(\\w+)@(\\w+)\\.com)      anchored -        floating ".com"',
        '(?^:\\b\\d{4}-\\d{2}-\\d{2}\\b) anchored "-"      floating -',
        '(?^:colou?r|grey)          anchored -        floating -',
    ]
    assert out.splitlines() == expected, out
