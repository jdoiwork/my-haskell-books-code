# The Python code in the search engine concurrency chapter.
# 検索エンジンを並行化する章の Python のコード
import re
from collections import Counter, defaultdict

STOP_WORDS = {"a", "an", "and", "the", "of", "on", "in", "to",
              "is", "it", "its", "at", "with", "for"}

def tokenize(text):
    pieces = re.split(r"[^0-9a-z]", text.lower())
    return [w for w in pieces if w and w not in STOP_WORDS]

def build_index(docs):
    postings = defaultdict(dict)
    for doc_id, title, body in docs:
        for t, n in Counter(tokenize(title + " " + body)).items():
            postings[t][doc_id] = n
    return dict(postings)

# ANCHOR: merge
# Python (merge two indexes of different documents)
# Python（別々の文書のインデックスを2つ合わせる）
def merge(a, b):
    out = {t: dict(ps) for t, ps in a.items()}
    for t, ps in b.items():
        out.setdefault(t, {}).update(ps)
    return out
# ANCHOR_END: merge

# ANCHOR: pool
# Python (processes, not threads: one GIL per process)
# Python（スレッドではなくプロセスで：GIL はプロセスごと）
from concurrent.futures import ProcessPoolExecutor
from functools import reduce

def chunks(xs, n):
    return [xs[i:i + n] for i in range(0, len(xs), n)]

def build_parallel(docs, size):
    with ProcessPoolExecutor() as pool:
        parts = list(pool.map(build_index, chunks(docs, size)))
    return reduce(merge, parts, {})
# ANCHOR_END: pool

# ANCHOR: main
# Call it only under __main__: each child process reads this file
# again, to find build_index by its name.
# 呼ぶのは __main__ の中だけ：子のプロセスは、build_index を名前で
# 探すために、このファイルを読み直す
if __name__ == "__main__":
    words = ["dragon", "fire", "gold"]
    docs = [(i, f"Doc {i}", " ".join(words[: i % 3 + 1]))
            for i in range(1, 101)]
    assert build_parallel(docs, 10) == build_index(docs)
    print("ok")
# ANCHOR_END: main
