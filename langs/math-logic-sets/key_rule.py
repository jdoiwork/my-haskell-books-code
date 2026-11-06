# The guild's key rule of the chapter "Logic and sets", in Python.
# 「論理と集合」の章のギルドの鍵の決まりを、Python で
heroes = [(15, False), (12, True), (5, True), (5, False)]


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def key_rule_ok(level, has_key):
    if has_key:
        return level >= 10
    # Nothing is said about heroes without a key.
    # 鍵を持たない人の話はしていない
    return True
# ANCHOR_END: procedural


assert [key_rule_ok(lv, k) for lv, k in heroes] == [True, True, False, True]


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def key_rule_ok(level, has_key):
    return not has_key or level >= 10
# ANCHOR_END: functional


assert [key_rule_ok(lv, k) for lv, k in heroes] == [True, True, False, True]
print("ok")
