# Remove the .log files that are 7 days old or more, in Python: a loop
# with os first, then pathlib. The tests run both in a folder of their
# own.
# 7日以上たった .log ファイルを消す。Python で：まず os のループ、
# 次に pathlib。テストは、自分のフォルダーで両方を動かす
import os
import tempfile
import time
from pathlib import Path

DAY = 24 * 60 * 60


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def cleanup(folder, days=7, dry_run=False):
    now = time.time()
    removed = []
    for name in sorted(os.listdir(folder)):
        if not name.endswith(".log"):
            continue
        path = os.path.join(folder, name)
        age = int((now - os.path.getmtime(path)) // DAY)
        if age >= days:
            if not dry_run:
                os.remove(path)
            removed.append(path)
    return removed
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def old_logs(folder, days=7):
    now = time.time()
    return [
        p
        for p in sorted(Path(folder).glob("*.log"))
        if (now - p.stat().st_mtime) // DAY >= days
    ]


def cleanup_paths(folder, days=7):
    olds = old_logs(folder, days)
    for p in olds:
        p.unlink()
    return olds
# ANCHOR_END: functional


def make_logs(folder):
    now = time.time()
    for name, age in [("a.log", 15.2), ("b.log", 7.1), ("c.log", 6.8),
                      ("d.log", 0), ("notes.txt", 30)]:
        path = os.path.join(folder, name)
        Path(path).write_text("x\n")
        os.utime(path, (now - age * DAY, now - age * DAY))


with tempfile.TemporaryDirectory() as d:
    make_logs(d)
    looked = cleanup(d, dry_run=True)
    assert [os.path.basename(p) for p in looked] == ["a.log", "b.log"]
    assert len(os.listdir(d)) == 5
    cleanup(d)
    assert sorted(os.listdir(d)) == ["c.log", "d.log", "notes.txt"]

with tempfile.TemporaryDirectory() as d:
    make_logs(d)
    assert [p.name for p in cleanup_paths(d)] == ["a.log", "b.log"]
    assert sorted(os.listdir(d)) == ["c.log", "d.log", "notes.txt"]

# find -mtime +6 picks the same files: more than 6 whole days.
# find -mtime +6 も同じファイルを選ぶ：まる6日より多い
import subprocess
with tempfile.TemporaryDirectory() as d:
    make_logs(d)
    found = subprocess.run(["find", d, "-name", "*.log", "-mtime", "+6"],
                           capture_output=True, text=True, check=True)
    assert sorted(os.path.basename(p) for p in found.stdout.split()) == \
        ["a.log", "b.log"], found.stdout
print("ok cleanup.py")
