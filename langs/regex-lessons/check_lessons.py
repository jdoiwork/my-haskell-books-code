# The chapter on learning from other lands (MiniRegex): ask Rust's
# regex-automata which strategy it chose for each of the guild's jobs,
# and Perl 5.44 what it looks for first. Prints both, as the chapter
# shows them, and checks them against the chapter's text.
# よその国に学ぶ章（MiniRegex）：ギルドの仕事ごとに、Rust の
# regex-automata が選んだ手と、Perl 5.44 が先に探すものを聞く。章と同じ
# 形で出し、章の文とくらべて確かめる
import os
import re
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
cargo = os.path.join(here, "..", "cargo")

# 🦀 Rust: the strategy of the meta engine.
# Rust：meta エンジンの手
subprocess.run(["cargo", "build", "-q", "--release", "--bin",
                "regex_lessons"], cwd=cargo, check=True)
rust = subprocess.run([os.path.join(cargo, "target", "release",
                                    "regex_lessons")],
                      capture_output=True, text=True, check=True).stdout
print(rust, end="")
assert rust == """dates
  using reverse inner strategy
mail
  using reverse inner strategy
names
  using prefilter teddy strategy
caseless
  using prefilter teddy strategy
sales
  using reverse inner strategy
""", rust


# 🧅 Perl: the fixed text it checks first, and the class a match starts
#    with. The lines are cut: the classes with Unicode are very long.
# Perl：先に確かめる決まった文字列と、一致の始まりの文字の種類。行は
# 切る：Unicode の文字の種類は、とても長い
def short(line):
    line = line.replace("9223372036854775807", "inf")
    line = re.sub(r" \(checking (floating|anchored)\)", "", line)
    m = re.match(r"(.*?stclass [A-Z-]+)", line)
    return m.group(1) if m else line


perl = "/opt/homebrew/bin/perl"
if not os.path.exists(perl):
    print("skip: no Perl 5.44 (brew install perl)")
else:
    err = subprocess.run([perl, os.path.join(here, "run", "intuit.pl")],
                         capture_output=True, text=True, check=True).stderr
    kept = [short(l) for l in err.splitlines()
            if l.startswith(("== ", "floating", "anchored", "stclass"))]
    out = "\n".join(kept) + "\n"
    print(out, end="")
    assert out == """== dates
anchored "-" at 4..4 stclass POSIXU
== mail
floating "@" at 1..inf stclass ANYOFD
== names
stclass AHOCORASICKC-EXACT
== caseless
stclass EXACTFU
== sales
floating " sold " at 1..inf stclass POSIXD
""", out
print("ok")
