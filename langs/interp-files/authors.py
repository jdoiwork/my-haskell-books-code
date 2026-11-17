# Count the commits of each person, in Python: subprocess.run with a
# loop first, then Counter. The tests make a git repository of their
# own.
# 人ごとのコミットを数える。Python で：まず subprocess.run とループ、
# 次に Counter。テストは、自分の git リポジトリーを作る
import os
import subprocess
import tempfile
from collections import Counter


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def authors(folder="."):
    # A list of words, no shell: like Process.run "git" [...].
    # 語のリストで、シェルなし：Process.run "git" [...] と同じ
    done = subprocess.run(["git", "log", "--format=%an"], cwd=folder,
                          capture_output=True, text=True)
    if done.returncode != 0:
        raise RuntimeError(f"git log ended with {done.returncode}")
    counts = {}
    for name in done.stdout.splitlines():
        counts[name] = counts.get(name, 0) + 1
    return sorted(counts.items(), key=lambda p: (-p[1], p[0]))
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def ranking(folder="."):
    out = subprocess.run(["git", "log", "--format=%an"], cwd=folder,
                         capture_output=True, text=True,
                         check=True).stdout     # not 0: an exception
                                                # 0 でなければ例外
    return Counter(out.splitlines()).most_common()
# ANCHOR_END: functional


with tempfile.TemporaryDirectory() as d:
    subprocess.run(["git", "init", "-q", d], check=True)
    for a in ["Ada", "Grace", "Ada", "Lam", "Ada", "Grace"]:
        subprocess.run(["git", "-c", f"user.name={a}",
                        "-c", f"user.email={a}@example.com",
                        "commit", "-q", "--allow-empty", "-m", "x"],
                       cwd=d, check=True)
    assert authors(d) == [("Ada", 3), ("Grace", 2), ("Lam", 1)]
    assert ranking(d) == [("Ada", 3), ("Grace", 2), ("Lam", 1)]

# ANCHOR: endings
# Python: the three endings are an exception, a code, and a code.
# Python：3つの終わり方は、例外・コード・コード
try:
    subprocess.run(["no-such-command"])
    raise AssertionError("it started")
except FileNotFoundError as e:          # cannot start
    assert e.filename == "no-such-command"
failed = subprocess.run(["ls", "no-such-file"], capture_output=True)
assert failed.returncode != 0           # it ended, but not with 0
try:
    subprocess.run(["ls", "no-such-file"], capture_output=True, check=True)
    raise AssertionError("check=True did not raise")
except subprocess.CalledProcessError as e:
    assert e.returncode == failed.returncode
# ANCHOR_END: endings
print("ok authors.py")
