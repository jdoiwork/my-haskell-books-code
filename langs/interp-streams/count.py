# Count the lines of each status of a big access log, one line at a
# time, in Python: a loop with a dict first, then Counter. Also: bytes
# that are not UTF-8, three ways, and the SHA-256 of a file, a little at
# a time. The tests make their own log in a folder of their own.
# 大きなアクセスログの行を、状態コードごとに1行ずつ数える。Python で：
# まず dict のループ、次に Counter。それと、UTF-8 でないバイトの3通りの
# 読み方と、ファイルの SHA-256 を少しずつ。テストは、自分のフォルダーに
# 自分でログを作る
import base64
import hashlib
import os
import tempfile
import uuid
import random
from collections import Counter


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def count_statuses(path):
    counts = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            parts = line.split()
            if len(parts) == 5:
                status = parts[3]
                counts[status] = counts.get(status, 0) + 1
    return counts
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def count_statuses_counter(path):
    with open(path, encoding="utf-8") as f:
        return Counter(
            parts[3] for parts in map(str.split, f) if len(parts) == 5
        )
# ANCHOR_END: functional


# ANCHOR: decode
# Python: one bytes, three ways to read the broken part
# Python：1つのバイト列を、壊れた所の3通りの読み方で
raw = bytes([99, 97, 102, 233])        # b'caf\xe9'
try:
    raw.decode("utf-8")                # errors="strict"
except UnicodeDecodeError as e:
    where = e.start                    # 3
replaced = raw.decode("utf-8", errors="replace")   # 'caf�'
ignored = raw.decode("utf-8", errors="ignore")     # 'caf'
# ANCHOR_END: decode


# ANCHOR: hash
# Python: the SHA-256 of a file, 64 KiB at a time
# Python：ファイルの SHA-256 を、64 KiB ずつ
def sha256_of(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        while chunk := f.read(65536):
            h.update(chunk)
    return h.hexdigest()


def sha256_of_digest(path):
    with open(path, "rb") as f:
        return hashlib.file_digest(f, "sha256").hexdigest()
# ANCHOR_END: hash


# ANCHOR: others
# Python: base64 and a UUID of version 4
# Python：base64 と、バージョン4の UUID
text = base64.b64encode("Lam!".encode("utf-8")).decode("ascii")   # 'TGFtIQ=='
back = base64.b64decode(text).decode("utf-8")                     # 'Lam!'
rng = random.Random(618)
made = uuid.UUID(bytes=rng.randbytes(16), version=4)
# ANCHOR_END: others


def main():
    assert where == 3
    assert replaced == "caf�"
    assert ignored == "caf"
    assert text == "TGFtIQ==" and back == "Lam!"
    assert made.version == 4 and str(made)[19] in "89ab"
    with tempfile.TemporaryDirectory() as d:
        log = os.path.join(d, "big.log")
        statuses = ["200", "200", "304", "404", "200", "500"]
        with open(log, "w", encoding="utf-8") as f:
            for i in range(200000):
                f.write(f"t GET / {statuses[i % 6]} 1ms\n")
        expected = {"200": 100001, "304": 33333, "404": 33333, "500": 33333}
        assert count_statuses(log) == expected
        assert count_statuses_counter(log) == Counter(expected)
        assert sha256_of(log) == sha256_of_digest(log)
        here = os.path.dirname(os.path.abspath(__file__))
        png = os.path.join(here, "..", "..", "lam", "streams", "examples",
                           "streams", "photos", "lam.png")
        assert sha256_of(png).startswith("4dcf4746ed52")
    print("ok")


if __name__ == "__main__":
    main()
