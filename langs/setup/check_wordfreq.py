# Run the Python wordfreq on the same sample as the Haskell version.
# Words with the same count may come in a different order (Haskell sorts
# them alphabetically), so compare the counts and the words above 2.
# Haskell 版と同じサンプルで、Python 版の wordfreq を動かす。
# 同じ回数の単語は並び順が違ってよい（Haskell はアルファベット順）ので、
# 回数の並びと、3回以上の単語を比べる
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
sample = os.path.join(here, "../../setup/wordfreq/sample.txt")
haskell = [("the", 6), ("you", 5), ("and", 3), ("haskell", 3), ("type", 3),
           ("a", 2), ("are", 2), ("do", 2), ("function", 2), ("in", 2)]
for name in ("wordfreq_procedural.py", "wordfreq_functional.py"):
    out = subprocess.run([sys.executable, os.path.join(here, "run", name)],
                         stdin=open(sample), capture_output=True, text=True,
                         check=True).stdout
    got = [(w, int(n)) for w, n in (line.split() for line in out.splitlines())]
    assert [n for _, n in got] == [n for _, n in haskell], (name, got)
    assert {p for p in got if p[1] > 2} == {p for p in haskell if p[1] > 2}, (name, got)
