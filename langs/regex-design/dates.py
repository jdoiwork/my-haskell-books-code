# The Python code in the design chapter of MiniRegex: find the dates in
# the records, first by hand, then with the re module; and how the
# answers of re look.
# MiniRegex の設計の章の Python のコード：記録から日付を探す。まず手で、
# 次に re モジュールで。そして re の答えの形
import re

records = """2026-11-19 Mira sold a dragon scale: 300 gold
2026-11-20 Ruby bought herb, sword, shield
On 2026-11-21 a dragon was seen near the lake
Room 1234-56-789 is closed
"""
want = ["2026-11-19", "2026-11-20", "2026-11-21", "1234-56-78"]


# ANCHOR: procedural
# Python (procedural style): look at 10 characters from each place
# Python（手続き型スタイル）：1文字ずつずらしながら、10文字を見る
def looks_like_date(d):
    return (len(d) == 10
            and d[0:4].isdigit() and d[4] == "-"
            and d[5:7].isdigit() and d[7] == "-"
            and d[8:10].isdigit())


def dates_by_hand(text):
    found = []
    i = 0
    while i + 10 <= len(text):
        if looks_like_date(text[i:i + 10]):
            found.append(text[i:i + 10])
            i += 10
        else:
            i += 1
    return found
# ANCHOR_END: procedural


assert dates_by_hand(records) == want

# ANCHOR: functional
# Python (functional style): the re module
# Python（関数型スタイル）：re モジュール
dates = re.findall(r"[0-9]{4}-[0-9]{2}-[0-9]{2}", records)
# ANCHOR_END: functional

assert dates == want

# ANCHOR: api
# Python: compile once, then search, find all, replace, split
# Python：一度コンパイルして、探す・全部探す・置き換える・切る
dragon = re.compile(r"dragon")
m = dragon.search(records)          # a Match, or None
print(m.group(), m.start(), m.end())
print(len(dragon.findall(records)))
print(dragon.sub("wyvern", records).splitlines()[0])
print(re.split(r", ", "herb, sword, shield"))
# ANCHOR_END: api

assert (m.group(), m.start(), m.end()) == ("dragon", 23, 29)
assert len(dragon.findall(records)) == 2
assert re.split(r", ", "herb, sword, shield") == ["herb", "sword", "shield"]

# ANCHOR: shapes
# Python: findall gives strings, or tuples when there are 2 groups
# Python：findall は文字列のリスト。グループが2つあるとタプルのリスト
print(re.findall(r"[0-9]{4}-[0-9]{2}", "2026-11 2027-01"))
print(re.findall(r"([0-9]{4})-([0-9]{2})", "2026-11 2027-01"))
# ANCHOR_END: shapes

assert re.findall(r"[0-9]{4}-[0-9]{2}", "2026-11 2027-01") == \
    ["2026-11", "2027-01"]
assert re.findall(r"([0-9]{4})-([0-9]{2})", "2026-11 2027-01") == \
    [("2026", "11"), ("2027", "01")]

# ANCHOR: error
# Python: a wrong pattern is found when it is compiled (re.error)
# Python：まちがったパターンは、コンパイルしたときにわかる（re.error）
try:
    re.compile("ab\\")
except re.error as e:
    print(e.msg, e.pos)
# ANCHOR_END: error

try:
    re.compile("ab\\")
    raise AssertionError("no error")
except re.error as e:
    assert (e.msg, e.pos) == ("bad escape (end of pattern)", 2)

# The empty pattern, between every character
# 空のパターンは、すべての文字のあいだに
assert re.split("", "abc") == ["", "a", "b", "c", ""]
