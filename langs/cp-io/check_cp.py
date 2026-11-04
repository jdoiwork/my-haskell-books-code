# Run the Python solutions on the same samples as the Haskell ones.
# Haskell 版と同じ入力例で、Python 版の解答を動かす
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
samples = os.path.join(here, "../../cpintro/samples")
cases = [
    ("sum.py", "sum.txt", "14 5"),
    ("coins.py", "coins.txt", "Yes"),
    ("coins.py", "coins-no.txt", "No"),
    ("treasure_procedural.py", "treasure.txt", "19"),
    ("treasure_procedural.py", "treasure-2.txt", "495"),
    ("treasure_functional.py", "treasure.txt", "19"),
    ("treasure_functional.py", "treasure-2.txt", "495"),
]
for prog, sample, expected in cases:
    out = subprocess.run([sys.executable, os.path.join(here, "run", prog)],
                         stdin=open(os.path.join(samples, sample)),
                         capture_output=True, text=True, check=True).stdout.strip()
    assert out == expected, (prog, sample, out)
