# The Python code in the higher-order chapter.
# 高階関数の章の Python のコード
enemies = [30, 5, 80, 12, 45]
hps = [5, 0, 55, 0, 20]
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
total = 0
for hp in hps:
    total = total + hp
# ANCHOR_END: procedural
assert total == 80
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
from functools import reduce

total = reduce(lambda acc, hp: acc + hp, hps, 0)
# ANCHOR_END: functional
assert total == 80
# ANCHOR: procedural_2
# Python (procedural style)
# Python（手続き型スタイル）
count = 0
total = 0
for hp in hps:
    if hp > 0:
        count += 1
        total += hp
# ANCHOR_END: procedural_2
assert (count, total) == (3, 80)
# ANCHOR: procedural_3
# Python (procedural style)
# Python（手続き型スタイル）
enemies = [30, 5, 80, 12, 45]

total = 0
for hp in enemies:
    after = max(0, hp - 25)
    if after > 0:
        total += after
# ANCHOR_END: procedural_3
assert total == 80
# ANCHOR: functional_2
# Python (functional style)
# Python（関数型スタイル）
after_attack = [max(0, hp - 25) for hp in enemies]
alive = [hp for hp in after_attack if hp > 0]
total = sum(alive)
# ANCHOR_END: functional_2
assert total == 80
# ANCHOR: functional_3
# Python (functional style)
# Python（関数型スタイル）
total = sum(
    filter(lambda hp: hp > 0,
           map(lambda hp: max(0, hp - 25), enemies)))
# ANCHOR_END: functional_3
assert total == 80
