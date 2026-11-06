# Run the sieve race of each language with a small n, and check the count.
# 各言語のふるいの競走を、小さい n で動かして、素数の数を確かめる
import os, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
node = os.environ.get("BOOK_NODE", "node")
rustc = os.environ.get("BOOK_RUSTC", "rustc")
dotnet = os.environ.get("BOOK_DOTNET", "dotnet")
clang = os.environ.get("BOOK_CLANG", "clang")
swiftc = os.environ.get("BOOK_SWIFTC", "swiftc")
go = os.environ.get("BOOK_GO", "go")
ocamlopt = os.environ.get("BOOK_OCAMLOPT", "ocamlopt")


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

# The compiled ones: build each into a temporary directory, then run it.
# コンパイルするもの：それぞれ一時的な場所に作ってから動かす
def build_commands(tmp):
    def exe(name):
        return os.path.join(tmp, name)

    def src(name):
        return os.path.join(run, name)

    return [
        ([rustc, "-O", "--edition", "2024", "-o", exe("rs"), src("race.rs")],
         exe("rs")),
        ([clang, "-O2", "-o", exe("c"), src("race.c")], exe("c")),
        ([swiftc, "-O", "-o", exe("swift"), src("race.swift")], exe("swift")),
        ([go, "build", "-o", exe("go"), src("race.go")], exe("go")),
        # ocamlopt writes .cmx files next to the source: copy it first.
        # ocamlopt はソースの隣に .cmx を作るので、先に写す
        (["cp", src("race.ml"), exe("race.ml")], None),
        ([ocamlopt, "-I", "+unix", "unix.cmxa", exe("race.ml"),
          "-o", exe("ml")], exe("ml")),
    ]


with tempfile.TemporaryDirectory() as tmp:
    for build, program in build_commands(tmp):
        subprocess.run(build, check=True, capture_output=True)
        if program:
            assert count([program, n]) == primes, program
race_cs = os.path.join(run, "race.cs")
assert count([dotnet, "run", "-c", "Release", race_cs, n]) == primes
print("ok")
