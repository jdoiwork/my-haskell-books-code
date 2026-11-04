# An append-only log in Python, for the chapter
# "Persistence: surviving a stop".
# 「永続化：落ちても戻れる」の章の、足していくだけのログを Python で
import json
import os
import tempfile


# ANCHOR: procedural
# Python (procedural style: one JSON line per command)
# Python（手続き型スタイル：コマンド1つにつき、JSON を1行）
def append(path, command):
    with open(path, "a") as f:
        f.write(json.dumps(command) + "\n")
        f.flush()
        os.fsync(f.fileno())          # really on the disk / 本当にディスクへ


def replay(path):
    store = {}
    with open(path) as f:
        for line in f:
            if not line.endswith("\n"):
                # cut short: leave it out / 切れていたら使わない
                break
            name, key, *args = json.loads(line)
            if name == "SET":
                store[key] = args[0]
            elif name == "DEL":
                store.pop(key, None)
    return store
# ANCHOR_END: procedural


path = os.path.join(tempfile.mkdtemp(), "log.jsonl")
append(path, ["SET", "hero", "Lambda"])
append(path, ["SET", "gold", "2"])
append(path, ["DEL", "gold"])
with open(path, "a") as f:
    # the server stopped here / ここで止まった
    f.write('["SET", "hero", "Pyth')
assert replay(path) == {"hero": "Lambda"}
print("ok")
