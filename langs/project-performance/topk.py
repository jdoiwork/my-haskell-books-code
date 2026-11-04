# The Python code in the search engine performance chapter.
# 検索エンジンを速くする章の Python のコード
import json
import pickle
import random

random.seed(1)
hits = [(random.random(), doc_id) for doc_id in range(3000)]

# ANCHOR: sortall
# Python (sort everything, then take 10)
# Python（全部を並べてから、10 件取る）
def top10_sorted(hits):
    return sorted(hits, key=lambda h: (-h[0], h[1]))[:10]
# ANCHOR_END: sortall

# ANCHOR: heap
# Python (heapq.nlargest: keeps only the best 10 while looking)
# Python（heapq.nlargest：見ながら、上位 10 件だけを持つ）
import heapq

def top10_heap(hits):
    return heapq.nlargest(10, hits, key=lambda h: (h[0], -h[1]))
# ANCHOR_END: heap

assert top10_sorted(hits) == top10_heap(hits)

# ANCHOR: formats
# Python (JSON is easy to read; pickle is fast, Python only)
# Python（JSON は読みやすい。pickle は速いが、Python 専用）
index = {"dragon": {1: 2, 3: 2, 6: 5}}
as_json = json.dumps(index)
as_pickle = pickle.dumps(index)
assert json.loads(as_json) == {"dragon": {"1": 2, "3": 2, "6": 5}}
assert pickle.loads(as_pickle) == index
# ANCHOR_END: formats
print("ok")
