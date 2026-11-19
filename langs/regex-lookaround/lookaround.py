# The Python code in the chapter on looking around of MiniRegex: the
# numbers before " gold", a lookbehind must have a fixed width, atomic
# groups and possessive quantifiers (3.11 and later), and \b.
# MiniRegex のまわりを見る章の Python のコード：" gold" の前の数、
# 後読みは決まった幅だけ、アトミックグループと所有的な量指定子
# （3.11 から）、\b
import re

ledger = "for 300 gold, a herb for 5 silver, price:1200 gold"

# ANCHOR: procedural
# Python (procedural style): look at each word and the word after it
# Python（手続き型スタイル）：言葉ごとに、うしろの言葉を見る
words = ledger.replace(",", "").split()
amounts = []
for i in range(len(words) - 1):
    if words[i].isdigit() and words[i + 1] == "gold":
        amounts.append(words[i])
print(amounts)
# ANCHOR_END: procedural

assert amounts == ["300"]

# ANCHOR: functional
# Python (functional style): look ahead for " gold", without taking it
# Python（関数型スタイル）：" gold" を先読みする。食べはしない
print(re.findall(r"\d+(?= gold)", ledger))
# ANCHOR_END: functional

assert re.findall(r"\d+(?= gold)", ledger) == ["300", "1200"]

# ANCHOR: behind
# Python: a lookbehind must always take the same number of characters
# Python：後読みは、いつも同じ文字数でなければならない
print(re.findall(r"(?<=price:)\d+", ledger))
for pattern in [r"(?<=price:\s?)\d+", r"(?<=colour|color)s"]:
    try:
        re.compile(pattern)
    except re.error as e:
        print(pattern, "->", e)
# ANCHOR_END: behind

assert re.findall(r"(?<=price:)\d+", ledger) == ["1200"]
for pattern in [r"(?<=price:\s?)\d+", r"(?<=colour|color)s"]:
    try:
        re.compile(pattern)
        raise AssertionError(pattern)
    except re.error as e:
        assert "fixed-width" in str(e)

# ANCHOR: atomic
# Python 3.11 and later: atomic groups and possessive quantifiers
# Python 3.11 から：アトミックグループと所有的な量指定子
print(re.search(r"(?>a|ab)c", "abc"), re.search(r"(?:a|ab)c", "abc"))
print(re.search(r"a*+a", "aaa"))
print(re.search(r"(?>a+)+b", "a" * 30 + "c"))
# ANCHOR_END: atomic

assert re.search(r"(?>a|ab)c", "abc") is None
assert re.search(r"(?:a|ab)c", "abc").group() == "abc"
assert re.search(r"a*+a", "aaa") is None
assert re.search(r"(?>a+)+b", "a" * 30 + "c") is None

# ANCHOR: boundary
# Python: \w and \b are Unicode for str; re.ASCII makes them ASCII
# Python：str なら \w も \b も Unicode。re.ASCII で ASCII に
print(re.findall(r"\bcafé\b", "a café."),
      re.findall(r"\bcafé\b", "a café.", re.ASCII))
# ANCHOR_END: boundary

assert re.findall(r"\bcafé\b", "a café.") == ["café"]
assert re.findall(r"\bcafé\b", "a café.", re.ASCII) == []
