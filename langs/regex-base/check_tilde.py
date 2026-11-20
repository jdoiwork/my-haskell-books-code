# Run the =~ of Perl, Bash and Groovy (in Docker, the official images)
# and the ~ of awk (macOS), and check what they print. Without Docker,
# the three are skipped with a message.
# Perl・Bash・Groovy の =~（Docker の公式のイメージ）と、awk の ~
# （macOS）を動かし、出力を確かめる。Docker がなければ、3つは知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold\n"


def docker(image, args, script):
    with open(os.path.join(here, script)) as f:
        return subprocess.run(
            ["docker", "run", "--rm", "--network", "none", "-i", image]
            + args,
            stdin=f, capture_output=True, text=True, check=True).stdout


def check(name, got, want):
    assert got == want, f"{name}: {got!r}"
    print(f"{name}: ok")


if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for Perl, Bash and Groovy")
else:
    check("perl", docker("perl:5.44-slim", ["perl", "-"], "context.pl"),
          "found: Mira 300\ngroups: Mira 300\nall: 300 25\ncount: 2\n")
    check("bash", docker("bash:5.3.20", ["bash", "-s"], "tilde.sh"),
          "status: 0\nfound: Mira 300\nno dragon: 1\n")
    # Groovy reads the script from a file: put it in through a volume
    # Groovy はファイルから読むので、ボリュームで入れる
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none",
         "-v", f"{here}:/work:ro", "-w", "/work", "groovy:6.0.0-jdk25",
         "groovy", "Tilde.groovy"],
        capture_output=True, text=True, check=True).stdout
    check("groovy", out,
          "type: Matcher\nfound: Mira 300\nall: [300, 25]\n"
          "whole: true false\n")

awk = subprocess.run(["/usr/bin/awk", "-f", os.path.join(here, "tilde.awk")],
                     input=line, capture_output=True, text=True,
                     check=True).stdout
check("awk", awk, "gold: 1 dragon: 0\nat: 22 8\n")
