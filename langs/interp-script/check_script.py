# Run the wordfreq scripts of the other languages, like the Lam one:
# with a file, with the standard input, and with a file that is not
# there (exit code 1, and a message on the standard error).
# The answer must be the same as the first chapter's Haskell.
# ほかの言語の wordfreq のスクリプトを、Lam 版と同じように動かす：
# ファイル、標準入力、ないファイル（終了コード 1、標準エラーにわけ）。
# 答えは、最初の章の Haskell 版と同じ
import os, shutil, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
sample = os.path.join(here, "../../setup/wordfreq/sample.txt")
node = os.environ.get("BOOK_NODE", "node")
haskell = ("the         6\nyou         5\nand         3\nhaskell     3\n"
           "type        3\na           2\nare         2\ndo          2\n"
           "function    2\nin          2\n")
commands = {
    "wordfreq.py": [sys.executable, os.path.join(run, "wordfreq.py")],
    "wordfreq_functional.py":
        [sys.executable, os.path.join(run, "wordfreq_functional.py")],
    "wordfreq.ts": [node, "--no-warnings", os.path.join(run, "wordfreq.ts")],
    "wordfreq.exs": ["elixir", os.path.join(run, "wordfreq.exs")],
}
for name, command in commands.items():
    with_file = subprocess.run(command + [sample], capture_output=True,
                               text=True, check=True).stdout
    assert with_file == haskell, (name, with_file)
    with open(sample) as f:
        piped = subprocess.run(command, stdin=f, capture_output=True,
                               text=True, check=True).stdout
    assert piped == haskell, (name, piped)
    missing = subprocess.run(command + ["nope.txt"], capture_output=True,
                             text=True)
    assert missing.returncode == 1, (name, missing.returncode)
    assert missing.stdout == "", (name, missing.stdout)
    assert missing.stderr.startswith("wordfreq: nope.txt: "), (name, missing.stderr)
    print("ok", name)

# exit, and the cleanups: Go's os.Exit and Python's os._exit skip them,
# Python's sys.exit runs finally on the way out. The exit code is 3.
# exit と後始末：Go の os.Exit と Python の os._exit は飛ばし、Python の
# sys.exit は出口で finally を動かす。終了コードは 3
# go run turns the code into 1 ("exit status 3"), so build it first.
# go run は終了コードを 1 にする（"exit status 3"）ので、先に作る
go = os.environ.get("BOOK_GO", "go")
tmp = tempfile.mkdtemp()
closing = os.path.join(tmp, "closing")
subprocess.run([go, "build", "-o", closing, os.path.join(run, "closing.go")],
               check=True)
opens = "the shop opens\n"
closes = opens + "the shop closes\n"
for name, command, expected in [
        ("closing.go", [closing], opens),
        ("closing.py", [sys.executable, os.path.join(run, "closing.py")], closes),
        ("closing.py hard",
         [sys.executable, os.path.join(run, "closing.py"), "hard"], opens)]:
    done = subprocess.run(command, capture_output=True, text=True)
    assert done.returncode == 3, (name, done)
    assert done.stdout == expected, (name, done.stdout)
    print("ok", name)
shutil.rmtree(tmp)
