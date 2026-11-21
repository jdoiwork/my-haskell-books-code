# The Python code in the emoji chapter of MiniRegex: len counts code
# points, unicodedata.iter_graphemes (new in 3.15) counts what a person
# sees, and re has neither \X nor \p.
# MiniRegex の絵文字の章の Python のコード：len はコードポイントを数え、
# unicodedata.iter_graphemes（3.15 で足された）は人が見る文字を数える。
# re には \X も \p もない
import os
import re
import unicodedata

# 👨‍👩‍👧: a man, a woman and a girl, with ZWJs between
# 👨‍👩‍👧：男の人、女の人、女の子のあいだに ZWJ
family = "\U0001F468‍\U0001F469‍\U0001F467"

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
print(len(family))
count = 0
for _ in unicodedata.iter_graphemes(family):
    count += 1
print(count)
# ANCHOR_END: procedural

assert len(family) == 5
assert count == 1

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
print(sum(1 for _ in unicodedata.iter_graphemes(family)))
print([str(g) for g in unicodedata.iter_graphemes("🇯🇵👍🏽")])
# ANCHOR_END: functional

assert [str(g) for g in unicodedata.iter_graphemes("🇯🇵👍🏽")] \
    == ["🇯🇵", "👍🏽"]

# ANCHOR: nore
# Python: re knows neither \X nor \p{...}
# Python：re は \X も \p{...} も知らない
for pattern in [r"\X", r"\p{Emoji}"]:
    try:
        re.compile(pattern)
    except re.PatternError as e:
        print(e)
# ANCHOR_END: nore

# Python cuts Unicode's own tests just as Unicode says (all 766)
# Python は、Unicode の公式のテストを、そのとおりに切る（766 問とも）
here = os.path.dirname(os.path.abspath(__file__))
tests = os.path.join(here, "..", "..", "miniregex", "emoji", "ucd",
                     "GraphemeBreakTest.txt")
cases = 0
with open(tests, encoding="utf-8") as f:
    for line in f:
        if not line.startswith("÷"):
            continue
        parts, now = [], ""
        for word in line.split("#")[0].split():
            if word == "÷":
                if now:
                    parts.append(now)
                now = ""
            elif word != "×":
                now += chr(int(word, 16))
        assert [str(g) for g in unicodedata.iter_graphemes("".join(parts))] \
            == parts, line
        cases += 1
assert cases == 766
