# The Python code in the groups chapter of MiniRegex: taking groups out,
# groups in a repeat, a reference to a group that found nothing, all the
# matches after an empty one, and replacement texts.
# MiniRegex のグループの章の Python のコード：グループを取り出す、
# くり返しの中のグループ、何も見つけなかったグループへの参照、空の一致の
# あとの全部の一致、置き換えの文字列
import re

records = """2026-11-19 Mira sold a dragon scale: 300 gold
On 2026-11-21 a dragon was seen near the lake
"""

# ANCHOR: procedural
# Python (procedural style): find the dates one by one, and take
#   the parts out by name
# Python（手続き型スタイル）：日付を1つずつ探して、部分を名前で取り出す
date = re.compile(r"(?P<year>[0-9]{4})-(?P<month>[0-9]{2})-(?P<day>[0-9]{2})")
parts = []
for m in date.finditer(records):
    parts.append((m.group("year"), m.group("month"), m.group("day")))
print(parts)
print(m.span("month"))
# ANCHOR_END: procedural

assert parts == [("2026", "11", "19"), ("2026", "11", "21")]
assert m.span("month") == (54, 56)

# ANCHOR: functional
# Python (functional style): a dict of the named groups for each match
# Python（関数型スタイル）：一致ごとに、名前付きのグループの dict
print([m.groupdict() for m in date.finditer(records)][0])
# ANCHOR_END: functional

assert [m.groupdict() for m in date.finditer(records)][0] == \
    {"year": "2026", "month": "11", "day": "19"}

# ANCHOR: unset
# Python: a group that took no part is None
# Python：使われなかったグループは None
print(re.search(r"(a)|(b)", "b").groups())
print(re.search(r"(?:(a)|b)*", "ab").groups())
# ANCHOR_END: unset

assert re.search(r"(a)|(b)", "b").groups() == (None, "b")
assert re.search(r"(?:(a)|b)*", "ab").groups() == ("a",)

# ANCHOR: backref
# Python: \1 of a group that found nothing does not match
# Python：何も見つけなかったグループの \1 は、合わない
print(re.search(r"(a)?\1", "b"))
print(re.search(r"([a-z]+) \1", "the the dragon").group())
# ANCHOR_END: backref

assert re.search(r"(a)?\1", "b") is None
assert re.search(r"([a-z]+) \1", "the the dragon").group() == "the the"

# ANCHOR: findall
# Python: after an empty match, a match that is not empty is tried at
#   the same place first (since Python 3.7)
# Python：空の一致のあと、同じ場所で空でない一致を先に試す
#   （Python 3.7 から）
print(re.findall(r"a??", "a"))
print(re.split(r"x*", "axbxx"))
# ANCHOR_END: findall

assert re.findall(r"a??", "a") == ["", "a", ""]
assert re.findall(r"x*", "axbxx") == ["", "x", "", "xx", ""]
assert re.split(r"x*", "axbxx") == ["", "a", "", "b", "", ""]

# ANCHOR: sub
# Python: \g<name> in a replacement text, or a function
# Python：置き換えの文字列の \g<name>、または関数
print(date.sub(r"\g<day>/\g<month>/\g<year>", "On 2026-11-19"))
print(date.sub(lambda m: f"{m['day']}/{m['month']}/{m['year']}",
               "On 2026-11-19"))
# ANCHOR_END: sub

assert date.sub(r"\g<day>/\g<month>/\g<year>", "On 2026-11-19") \
    == "On 19/11/2026"
# a group that is not there is an error; one that found nothing is ""
# ないグループはエラー。何も見つけなかったグループは ""
try:
    re.sub(r"(a)", r"\2", "a")
    assert False
except re.error:
    pass
assert re.sub(r"(a)|(b)", r"<\2>", "ab") == "<><b>"
