# The Python code in the search engine design chapter.
# 検索エンジンの設計の章の Python のコード
import math
import re

STOP_WORDS = {"a", "an", "and", "the", "of", "on", "in", "to",
              "is", "it", "its", "at", "with", "for"}

def tokenize(text):
    # Cut at anything that is not a letter or a digit (Unicode too).
    # 文字でも数字でもないところで切る（Unicode の文字も）
    pieces = re.split(r"[\W_]", text.lower())
    return [w for w in pieces if w and w not in STOP_WORDS]

# ANCHOR: procedural
# Python (procedural style: dicts, filled by loops)
# Python（手続き型スタイル：辞書を、ループで埋める）
def build_index(docs):
    postings = {}   # token -> {doc_id -> count}
    lengths = {}    # doc_id -> number of tokens
    for doc_id, title, body in docs:
        tokens = tokenize(title + " " + body)
        lengths[doc_id] = len(tokens)
        for t in tokens:
            if t not in postings:
                postings[t] = {}
            postings[t][doc_id] = postings[t].get(doc_id, 0) + 1
    return postings, lengths
# ANCHOR_END: procedural

# ANCHOR: functional
# Python (functional style: Counter and defaultdict)
# Python（関数型スタイル：Counter と defaultdict）
from collections import Counter, defaultdict

def build_index_fn(docs):
    postings = defaultdict(dict)
    lengths = {}
    for doc_id, title, body in docs:
        tokens = tokenize(title + " " + body)
        lengths[doc_id] = len(tokens)
        for t, n in Counter(tokens).items():
            postings[t][doc_id] = n
    return postings, lengths

def search(postings, lengths, query):
    tokens = list(dict.fromkeys(tokenize(query)))
    if not tokens:
        return []
    common = set.intersection(
        *(set(postings.get(t, {})) for t in tokens))
    n = len(lengths)

    def score(d):
        return sum(postings[t][d] / lengths[d]
                   * math.log(1 + n / len(postings[t]))
                   for t in tokens)

    return sorted(common, key=lambda d: (-score(d), d))
# ANCHOR_END: functional

docs = [
    (1, "Red Dragon", "The red dragon sleeps on a mountain of gold. "
     "Its fire melts iron, and its roar shakes the valley."),
    (2, "Slime", "A slime is soft and blue. It eats moss in the cave "
     "and splits in two when you hit it."),
    (3, "Ice Dragon", "The ice dragon lives at the top of the frozen "
     "mountain. Fire is its weakness."),
    (4, "Fire Cave", "Lava flows in the fire cave. "
     "Bring water and an ice shield."),
    (5, "Goblin Camp", "Goblins steal gold from travelers "
     "and hide it in the forest."),
    (6, "Dragon Tamer", "A dragon tamer speaks to a dragon with a flute. "
     "A calm dragon is a friend, an angry dragon is fire."),
]

assert tokenize("The red Dragon, of fire!") == ["red", "dragon", "fire"]
assert tokenize("café 赤いドラゴンが炎を吐く") == ["café", "赤いドラゴンが炎を吐く"]
p1, l1 = build_index(docs)
p2, l2 = build_index_fn(docs)
assert p1 == dict(p2) and l1 == l2
assert p1["dragon"] == {1: 2, 3: 2, 6: 5}
assert search(p2, l2, "dragon fire") == [6, 3, 1]
assert search(p2, l2, "gold") == [5, 1]
assert search(p2, l2, "unicorn") == []
print("ok")
