# Python's re chooses among the matches by the same rules as PCRE2 (and
# so as MiniRegex's Pike VM): the left branch first, and an empty round
# of a repeat sets the group.
# Python の re も、PCRE2 と同じ決まりで一致を選ぶ（だから MiniRegex の
# Pike VM とも同じ）：左の枝が先、くり返しの空の回もグループを書く
# ANCHOR: cases
import re

cases = [
    (r"a|ab", "ab"),
    (r"(a|)*b", "aab"),
    (r"(a|b|)+?c", "abc"),
]
# ANCHOR_END: cases

# ANCHOR: procedural
# Python (procedural style): what re finds, and its groups
# Python（手続き型スタイル）：re が見つけるものと、グループ
answers = []
for pattern, text in cases:
    m = re.search(pattern, text)
    answers.append((pattern, m.group(), m.groups()))
    print(pattern, repr(m.group()), m.groups())
# ANCHOR_END: procedural

# ANCHOR: functional
# Python (functional style): the same, in a comprehension
# Python（関数型スタイル）：同じものを、内包表記で
found = [(p, m.group(), m.groups())
         for p, t in cases
         if (m := re.search(p, t))]
# ANCHOR_END: functional

assert answers == found
assert found == [
    ("a|ab", "a", ()),
    ("(a|)*b", "aab", ("",)),
    ("(a|b|)+?c", "abc", ("b",)),
]
