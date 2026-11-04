# The Python code in the expressions chapter.
# 式・型・関数の章の Python のコード
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def status(hp):
    if hp <= 0:
        result = "Defeated!"
    else:
        result = "Still standing"
    return result
# ANCHOR_END: procedural
assert status(0) == "Defeated!" and status(5) == "Still standing"
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def status(hp):
    return "Defeated!" if hp <= 0 else "Still standing"
# ANCHOR_END: functional
assert status(-3) == "Defeated!" and status(1) == "Still standing"
# damage from the Haskell code: at least 1
# Haskell のコードと同じ damage（最低 1）
def damage(attack, defense):
    return max(1, attack - defense)
# ANCHOR: procedural_2
# Python (procedural style)
# Python（手続き型スタイル）
import random

def attack_with_luck(attack, defense):
    d = damage(attack, defense)
    # 10% chance of a critical hit
    # 10% の確率で会心の一撃
    if random.random() < 0.1:
        d = d * 2
    return d
# ANCHOR_END: procedural_2
assert all(attack_with_luck(50, 20) in (30, 60) for _ in range(100))
# ANCHOR: functional_2
# Python (functional style)
# Python（関数型スタイル）
def critical(is_critical, d):
    return d * 2 if is_critical else d
# ANCHOR_END: functional_2
assert critical(True, 30) == 60 and critical(False, 30) == 30
# ANCHOR: procedural_3
# Python (procedural style)
# Python（手続き型スタイル）
def hero_attack(defense):
    return damage(50, defense)

results = []
for d in [10, 30, 60]:
    results.append(hero_attack(d))
# ANCHOR_END: procedural_3
assert results == [40, 20, 1]
# ANCHOR: functional_3
# Python (functional style)
# Python（関数型スタイル）
from functools import partial

hero_attack = partial(damage, 50)
results = list(map(hero_attack, [10, 30, 60]))
# ANCHOR_END: functional_3
assert results == [40, 20, 1]
