# Run the one-liners of Perl, Ruby, awk and Python in run/, each block
# in a fresh copy of the example files of Lam's one-liner chapter, and
# compare with what Lam gives (code/lam/oneliner/examples/oneliner/<name>.out).
# Perl 5.44 runs in Docker (the official image perl:5.44-slim); without
# Docker, Perl is skipped with a message.
# run/ の Perl・Ruby・awk・Python のワンライナーを、ブロックごとに、Lam の
# ワンライナーの章の例のファイルの新しい写しの中で動かし、Lam の答え
# （<名前>.out）とくらべる。Perl 5.44 は Docker の中で（公式のイメージ）
import os, re, shutil, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
lam = os.path.join(here, "../../lam/oneliner/examples/oneliner")
data = ["app.log", "notes.txt", "sales.csv", "scores.txt", "fruits.txt"]
perl_image = "perl:5.44-slim"

# Where the others print a little differently from Lam.
# ほかの言語の出し方が、Lam と少しちがうところ
differs = {
    # Lam groups the digits ({acc:,}); the others print 2820.
    # Lam は {acc:,} で3けたごとに区切る
    "total": "total 2820\n",
    # awk keeps the first of each line, in order; Lam sorts (Map.keys).
    # awk は、最初に出てきた順に残す。Lam は並べる
    ("awk", "unique"): "pear\napple\nfig\n",
}


def blocks(path):
    """The ANCHOR blocks of a script: (name, the command)."""
    text = open(path).read()
    # The blocks with no ANCHOR inside (not the "book" around them).
    # 中に ANCHOR のないブロック（まわりの "book" ではなく）
    return re.findall(r"# ANCHOR: (\w+)\n((?:(?!# ANCHOR).)*?)# ANCHOR_END: \1",
                      text, re.S)


def expected(lang, name):
    """Lam's answer: the standard output, and the files -i changed."""
    if (lang, name) in differs:
        return differs[(lang, name)], {}
    if name in differs:
        return differs[name], {}
    base = "grep" if name == "grepLoop" else name
    out = open(os.path.join(lam, base + ".out")).read()
    parts = re.split(r"^== examples/oneliner/(\S+) ==\n", out, flags=re.M)
    files = dict(zip(parts[1::2], parts[2::2]))
    return parts[0], files


def run(lang, command, work):
    if lang == "perl":
        return subprocess.run(
            ["docker", "run", "--rm", "--network", "none",
             "-v", work + ":/w", "-w", "/w", perl_image, "sh", "-c", command],
            capture_output=True, text=True)
    env = dict(os.environ)
    env["PATH"] = (os.path.dirname(os.path.abspath(sys.executable))
                   + os.pathsep + env["PATH"])
    return subprocess.run(["sh", "-c", command], cwd=work, env=env,
                          capture_output=True, text=True)


docker = subprocess.run(["docker", "info"], capture_output=True).returncode == 0
if docker:
    subprocess.run(["docker", "pull", "-q", perl_image],
                   capture_output=True, check=True)
for lang in ["perl", "ruby", "awk", "python"]:
    if lang == "perl" and not docker:
        print("skip perl (no Docker)")
        continue
    for name, command in blocks(os.path.join(here, "run", lang + ".sh")):
        work = tempfile.mkdtemp()
        try:
            for f in data:
                shutil.copy(os.path.join(lam, f), work)
            done = run(lang, command, work)
            want_out, want_files = expected(lang, name)
            assert done.returncode == 0, (lang, name, done.stderr)
            assert done.stderr == "", (lang, name, done.stderr)
            assert done.stdout == want_out, (lang, name, done.stdout)
            for f, text in want_files.items():
                got = open(os.path.join(work, f)).read()
                assert got == text, (lang, name, f, got)
        finally:
            shutil.rmtree(work)
        print("ok", lang, name)
