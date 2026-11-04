# The party's skills of the chapter "Logic and sets", in Python.
# 「論理と集合」の章のパーティーのスキルを、Python で
needed = {"heal", "fire", "stealth"}
members = [{"fire", "sword"}, {"heal", "fire"}, {"fire", "shield"}]

# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
missing = []
for skill in needed:
    found = False
    for member in members:
        if skill in member:
            found = True
    if not found:
        missing.append(skill)
# ANCHOR_END: procedural
assert missing == ["stealth"]

# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
party = set().union(*members)          # union / 和集合
missing = needed - party               # difference / 差集合
shared = set.intersection(*members)    # intersection / 共通部分
everyone_fights = all("fire" in m for m in members)
# ANCHOR_END: functional
assert missing == {"stealth"}
assert shared == {"fire"}
assert everyone_fights
assert all(x > 0 for x in []) and not any(x > 0 for x in [])
print("ok")
