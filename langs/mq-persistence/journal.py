# The Python code in the "persistence" chapter of the message broker part.
# メッセージブローカーの部の「永続化」の章の Python のコード
import json
import os
import tempfile


# ANCHOR: append
# Python (one JSON value per line, then flush and fsync)
# Python（1行に JSON を1つ。書いたら flush と fsync）

# ✍️ Add entries to the end of the journal.
# 日誌の最後に、行を書き足す
def append(path, entries):
    with open(path, "a", encoding="utf-8") as f:
        for entry in entries:
            f.write(json.dumps(entry) + "\n")
        f.flush()             # Python's buffer -> the OS / Python から OS へ
        os.fsync(f.fileno())  # the OS -> the disk / OS からディスクへ
# ANCHOR_END: append


# ANCHOR: replay
# Python (read every line; a line cut short is skipped)
# Python（全部の行を読む。途中で切れた行は読み飛ばす）

# 📖 Turn the lines back into the letters still waiting.
# 行を読み戻して、まだ待っている手紙にする
def replay(lines):
    entries = []
    for line in lines:
        try:
            entries.append(json.loads(line))
        except json.JSONDecodeError:
            pass  # cut short by a crash / 落ちて途中で切れた行
    finished = {e["finished"] for e in entries if "finished" in e}
    return [e["stored"] for e in entries
            if "stored" in e and e["stored"][1] not in finished]
# ANCHOR_END: replay


def load(path):
    with open(path, encoding="utf-8") as f:
        return replay(f)


# ANCHOR: compact
# Python (write a new file next to the old one, then swap them)
# Python（古いファイルの隣に新しいものを書いてから、入れ替える）

# 🧹 Keep only the letters still waiting.
# まだ待っている手紙だけを残す
def compact(path):
    live = [{"stored": letter} for letter in load(path)]
    fresh = path + ".new"
    with open(fresh, "w", encoding="utf-8") as f:
        for entry in live:
            f.write(json.dumps(entry) + "\n")
        f.flush()
        os.fsync(f.fileno())
    os.replace(fresh, path)  # all or nothing / 全部か、何もしないか
# ANCHOR_END: compact


with tempfile.TemporaryDirectory() as tmp:
    path = os.path.join(tmp, "journal.jsonl")
    append(path, [
        {"stored": ["dragon-team", 1, "A red dragon at the north gate"]},
        {"stored": ["herb-team", 2, "Ten moon herbs, please"]},
        {"stored": ["dragon-team", 3, "A baby dragon is lost"]},
    ])
    append(path, [{"finished": 1}])
    waiting = load(path)
    assert waiting == [
        ["herb-team", 2, "Ten moon herbs, please"],
        ["dragon-team", 3, "A baby dragon is lost"],
    ]

    # A crash in the middle of a line: the cut line is skipped.
    # 行の途中で落ちた：切れた行は読み飛ばされる
    cut = json.dumps({"stored": ["archive", 4, "Old map"]})
    with open(path, "a", encoding="utf-8") as f:
        f.write(cut[: len(cut) // 2])
    assert load(path) == waiting

    before = os.path.getsize(path)
    compact(path)
    assert os.path.getsize(path) < before
    assert load(path) == waiting
    assert not os.path.exists(path + ".new")
    with open(path, encoding="utf-8") as f:
        assert len(f.readlines()) == 2
print("ok")
