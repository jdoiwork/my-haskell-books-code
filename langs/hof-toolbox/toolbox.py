# The Python code in the hof-toolbox chapter.
# 高階関数の道具箱の章の Python のコード
attacks = [50, 35, 60]
defenses = [20, 40, 10]
hp_log = [100, 70, 40, 10, 0, 0]
battle_log = [(1, "Goblin-A"), (1, "Goblin-B"), (2, "Goblin-C"),
              (3, "Goblin-D"), (3, "Goblin-E")]

def damage(attack, defense):
    return max(1, attack - defense)
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
results = []
for i in range(len(attacks)):
    results.append(damage(attacks[i], defenses[i]))
# ANCHOR_END: procedural
assert results == [30, 1, 50]
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
results = [damage(a, d) for a, d in zip(attacks, defenses)]
# ANCHOR_END: functional
assert results == [30, 1, 50]
# ANCHOR: procedural_2
# Python (procedural style)
# Python（手続き型スタイル）
i = 0
while i < len(hp_log) and hp_log[i] > 0:
    i += 1
alive, after = hp_log[:i], hp_log[i:]
# ANCHOR_END: procedural_2
assert (alive, after) == ([100, 70, 40, 10], [0, 0])
# ANCHOR: functional_2
# Python (functional style)
# Python（関数型スタイル）
from itertools import dropwhile, takewhile

alive = list(takewhile(lambda hp: hp > 0, hp_log))
after = list(dropwhile(lambda hp: hp > 0, hp_log))
# ANCHOR_END: functional_2
assert (alive, after) == ([100, 70, 40, 10], [0, 0])
# ANCHOR: procedural_3
# Python (procedural style)
# Python（手続き型スタイル）
def boss_hp_history(hit, start_hp):
    history = []
    hp = start_hp
    while hp > 0:
        history.append(hp)
        hp -= hit
    return history
# ANCHOR_END: procedural_3
assert boss_hp_history(30, 100) == [100, 70, 40, 10]
# ANCHOR: functional_3
# Python (functional style)
# Python（関数型スタイル）
from itertools import count, takewhile

history = list(takewhile(lambda hp: hp > 0, count(100, -30)))
# ANCHOR_END: functional_3
assert history == [100, 70, 40, 10]
# ANCHOR: procedural_4
# Python (procedural style)
# Python（手続き型スタイル）
groups = []
for entry in battle_log:
    if groups and groups[-1][0][0] == entry[0]:
        groups[-1].append(entry)
    else:
        groups.append([entry])
# ANCHOR_END: procedural_4
assert [len(g) for g in groups] == [2, 1, 2]
# ANCHOR: functional_4
# Python (functional style)
# Python（関数型スタイル）
from itertools import groupby

groups = [list(g) for _, g in groupby(battle_log, key=lambda e: e[0])]
# ANCHOR_END: functional_4
assert [len(g) for g in groups] == [2, 1, 2]
