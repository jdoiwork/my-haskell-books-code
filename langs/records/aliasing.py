# The Python code in the records chapter's column on immutable values.
# レコードと newtype の章の、書き換えられない値のコラムの Python のコード

# ANCHOR: aliasing
# Python (procedural style)
# Python（手続き型スタイル）
party_hp = [100, 80]
# meant as a copy / コピーのつもり
backup = party_hp
# the hero is down / 勇者が倒れた
party_hp[0] = 0
# [0, 80]: the backup changed too! / 控えまで変わった！
print(backup)
# ANCHOR_END: aliasing
assert backup == [0, 80]

# ANCHOR: tuple
# Python (functional style)
# Python（関数型スタイル）
# a tuple cannot be changed / タプルは書き換えられない
party_hp = (100, 80)
backup = party_hp
# make a new one / 新しく作る
party_hp = (0,) + party_hp[1:]
# (100, 80): the backup is safe / 控えは無事
print(backup)
# ANCHOR_END: tuple
assert backup == (100, 80) and party_hp == (0, 80)
