# Run the sieve race of each language with a small n, and check the count.
# 各言語のふるいの競走を、小さい n で動かして、素数の数を確かめる
import os, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
node = os.environ.get("BOOK_NODE", "node")
rustc = os.environ.get("BOOK_RUSTC", "rustc")


def count(cmd):
    out = subprocess.run(cmd, capture_output=True, text=True,
                         check=True).stdout
    return int(out.split()[0])


# 78498 primes up to one million / 100 万までの素数は 78498 個
n, primes = "1000000", 78498
race_py = os.path.join(run, "race.py")
assert count([sys.executable, race_py, "list", n]) == primes
assert count([sys.executable, race_py, "numpy", n]) == primes
assert count([node, "--no-warnings", os.path.join(run, "race.ts"), n]) == primes
with tempfile.TemporaryDirectory() as tmp:
    exe = os.path.join(tmp, "race")
    subprocess.run([rustc, "-O", "--edition", "2024", "-o", exe,
                    os.path.join(run, "race.rs")], check=True)
    assert count([exe, n]) == primes
print("ok")
