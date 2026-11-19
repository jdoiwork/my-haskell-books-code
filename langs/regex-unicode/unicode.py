# The Python code in the Unicode chapter of MiniRegex: \d is Unicode,
# re.ASCII, the cases of i and ı, flags in the pattern, and places
# counted in characters.
# MiniRegex の Unicode の章の Python のコード：\d は Unicode、re.ASCII、
# i と ı の大文字と小文字、パターンの中のフラグ、文字で数える場所
import re

record = "２０２６年１１月１９日、2026-11-19"

# ANCHOR: procedural
# Python (procedural style): \d finds full-width digits too; with
#   re.ASCII only 0 to 9
# Python（手続き型スタイル）：\d は全角の数字も見つける。re.ASCII なら
#   0 から 9 だけ
years = []
for m in re.finditer(r"\d{4}", record):
    years.append(m.group())
print(years)
print(re.findall(r"\d{4}", record, re.ASCII))
# ANCHOR_END: procedural

assert years == ["２０２６", "2026"]
assert re.findall(r"\d{4}", record, re.ASCII) == ["2026"]

# ANCHOR: functional
# Python (functional style): int() reads full-width digits as they are
# Python（関数型スタイル）：int() は全角の数字も、そのまま読める
print([int(y) for y in re.findall(r"\d{4}", record)])
# ANCHOR_END: functional

assert [int(y) for y in re.findall(r"\d{4}", record)] == [2026, 2026]

# ANCHOR: caseless
# Python: under re.I, K (Kelvin) is k, and ß is ẞ, but not SS;
#   İ and ı are i and I, too (PCRE2 says no to these two)
# Python：re.I のもとで、K（ケルビン）は k、ß は ẞ（SS ではない）。
#   İ と ı も i と I になる（PCRE2 は、この2つは「ちがう」と言う）
for pat, text in [("k", "K"), ("ß", "ẞ"), ("ß", "SS"),
                  ("i", "İ"), ("ı", "I")]:
    print(pat, text, re.fullmatch(pat, text, re.I) is not None)
# ANCHOR_END: caseless

assert [re.fullmatch(p, t, re.I) is not None
        for p, t in [("k", "K"), ("ß", "ẞ"), ("ß", "SS"),
                     ("i", "İ"), ("ı", "I")]] == \
    [True, True, False, True, True]

# ANCHOR: flags
# Python: (?i:...) works for a part, but (?i) must be at the very start
# Python：(?i:...) は一部分にだけ効く。でも (?i) は、いちばん頭にしか
#   書けない
print(re.fullmatch(r"(?i:a)b", "Ab") is not None)
try:
    re.compile(r"a(?i)b")
except re.error as e:
    print(e)
# ANCHOR_END: flags

assert re.fullmatch(r"(?i:a)b", "Ab") is not None
try:
    re.compile(r"a(?i)b")
    raise AssertionError("compiled")
except re.error as e:
    assert "global flags not at the start" in str(e)

# ANCHOR: places
# Python: a str counts characters (code points), as MiniRegex does;
#   the bytes of UTF-8 are another count
# Python：str は文字（コードポイント）で数える。MiniRegex と同じ。
#   UTF-8 のバイトで数えると、ちがう数になる
text = "名前: Lam"
m = re.search("Lam", text)
print(m.span(), len(text[:m.start()].encode("utf-8")))
# ANCHOR_END: places

assert m.span() == (4, 7)
assert len(text[:m.start()].encode("utf-8")) == 8

# Python's re has no \p{...} (the regex module from PyPI has it)
# Python の re には \p{...} がない（PyPI の regex モジュールにはある）
try:
    re.compile(r"\p{Han}")
    raise AssertionError("compiled")
except re.error:
    pass
