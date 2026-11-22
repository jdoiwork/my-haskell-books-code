# A file name with a ; in it, given to wc in three ways: a list of
# words (no shell), a string for the shell, and a string for the shell
# with shlex.quote.
# ; の入ったファイルの名前を、3つのやり方で wc に渡す：語のリスト（シェル
# なし）、シェルに渡す文字列、shlex.quote したシェルに渡す文字列
import os
import shlex
import subprocess
import tempfile

os.chdir(tempfile.mkdtemp())
name = "notes; echo INJECTED.txt"
with open(name, "w") as f:
    f.write("one\ntwo\n")

# ANCHOR: list
# Python (a list of words): no shell reads the name
# Python（語のリスト）：名前を読むシェルはない
done = subprocess.run(["wc", "-l", name], capture_output=True, text=True)
print(done.stdout.strip())
# ANCHOR_END: list
assert done.stdout.split() == ["2", "notes;", "echo", "INJECTED.txt"]

# ANCHOR: shell
# Python (shell=True): the shell reads the string again; the ; ends wc
# Python（shell=True）：シェルが文字列を読み直す。; で wc が終わる
oops = subprocess.run(f"wc -l {name}", shell=True,
                      capture_output=True, text=True)
print(oops.stdout.strip())
# ANCHOR_END: shell
assert oops.stdout.strip() == "INJECTED.txt"

# ANCHOR: quote
# Python (shlex.quote): put it in '...' for the shell
# Python（shlex.quote）：シェルのために '…' に入れる
quoted = subprocess.run(f"wc -l {shlex.quote(name)}", shell=True,
                        capture_output=True, text=True)
print(shlex.quote(name))
# ANCHOR_END: quote
assert shlex.quote(name) == "'notes; echo INJECTED.txt'"
assert quoted.stdout.split() == done.stdout.split()
