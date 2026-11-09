# The review site's small computations in Python: the tag cloud with
# Counter, the Jaccard index with sets, and CSV with the csv module.
# For comparing with Tsundoku.Review.
# レビューサイトの小さな計算を Python で：Counter でタグの雲、集合で
# Jaccard 係数、csv モジュールで CSV。Tsundoku.Review とくらべるため
import csv
import io
from collections import Counter
from fractions import Fraction

shelf = [
    {"id": 1, "title": "はじめてのドラゴン学", "tags": {"竜", "図鑑", "魔法"}},
    {"id": 2, "title": "図書室の司書に転生した件", "tags": {"転生", "図書館", "魔法"}},
    {"id": 3, "title": "迷宮図書館の写本係", "tags": {"図書館", "迷宮", "魔法"}},
]


# ANCHOR: procedural
# Python (procedural style: count the tags with a dictionary)
# Python（手続き型スタイル：辞書でタグを数える）
def tag_cloud_loop(works):
    counts = {}
    for work in works:
        for tag in work["tags"]:
            counts[tag] = counts.get(tag, 0) + 1
    return sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: Counter, sets, and Fraction)
# Python（関数型スタイル：Counter、集合、Fraction）
def tag_cloud(works):
    counts = Counter(t for w in works for t in w["tags"])
    return sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))


def jaccard(a, b):
    both = a | b
    return Fraction(len(a & b), len(both)) if both else Fraction(0)
# ANCHOR_END: functional


assert tag_cloud(shelf) == tag_cloud_loop(shelf)
assert tag_cloud(shelf)[0] == ("魔法", 3)
assert jaccard(shelf[1]["tags"], shelf[2]["tags"]) == Fraction(2, 4)
assert jaccard(set(), set()) == 0

# ANCHOR: csv
# Python (the csv module quotes what needs quoting, as RFC 4180 says)
# Python（csv モジュールは、くるむべきものをくるむ。RFC 4180 のとおり）
out = io.StringIO()
writer = csv.writer(out)
writer.writerow(["id", "title", "tags"])
writer.writerow([3, '写本係は「しーっ」と"言った", と', "図書館 迷宮"])
# ANCHOR_END: csv

assert out.getvalue().splitlines()[1] == \
    '3,"写本係は「しーっ」と""言った"", と",図書館 迷宮'
assert next(csv.reader(io.StringIO(out.getvalue().splitlines()[1])))[1] == \
    '写本係は「しーっ」と"言った", と'
print("ok")
