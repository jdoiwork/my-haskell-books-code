# Python's re reads its pattern when the program runs: a wrong pattern
# raises re.PatternError (with the place), and a group name with a typo
# raises IndexError. Compare with [re|…|] and [typed|…|] of
# regex-polish.md, which stop the build.
# Python の re は、プログラムが動くときにパターンを読む。まちがった
# パターンは re.PatternError（場所つき）、打ちまちがえたグループの名前は
# IndexError。regex-polish.md の [re|…|] と [typed|…|] は、ビルドを止める
import re

# ANCHOR: typos
# Python
try:
    re.compile(r"(?P<year>\d{4})-(?P<month>\d{2}")
except re.PatternError as e:
    print(e)

m = re.search(r"(?P<year>\d{4})-(?P<month>\d{2})", "since 2026-11")
print(m["year"])
try:
    print(m["yaer"])
except IndexError as e:
    print(e)
# ANCHOR_END: typos

try:
    re.compile(r"(?P<year>\d{4})-(?P<month>\d{2}")
    raise AssertionError("no error")
except re.PatternError as e:
    assert e.pos == 16, e.pos
    assert str(e) == "missing ), unterminated subpattern at position 16", e
assert m["year"] == "2026"
