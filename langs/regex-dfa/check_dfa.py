# The chapter on the lazy DFA (MiniRegex): run Rust's lazy DFA
# (regex-automata's hybrid::regex::Regex) on the guild's sales log, and check
# the counts and the sizes of the caches against the chapter's text.
# lazy DFA の章（MiniRegex）：Rust の lazy DFA（regex-automata の
# hybrid::regex::Regex）をギルドの売り上げのログで動かし、件数と
# キャッシュの大きさを章の文とくらべて確かめる
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
cargo = os.path.join(here, "..", "cargo")
subprocess.run(["cargo", "build", "-q", "--release", "--bin", "regex_dfa"],
               cwd=cargo, check=True)
out = subprocess.run([os.path.join(cargo, "target", "release", "regex_dfa")],
                     capture_output=True, text=True, check=True).stdout
print(out, end="")
assert out == """dates     2000 matches, caches  11747 +  28736 bytes
mail       286 matches, caches  21538 +  94488 bytes
names     4000 matches, caches   7697 +   8894 bytes
caseless   536 matches, caches   1730 +   1700 bytes
sales     2000 matches, caches  20373 +  71704 bytes
""", out
print("ok")
