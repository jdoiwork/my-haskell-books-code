# The Python code in the records chapter.
# レコードと newtype の章の Python のコード
from enum import Enum

class Element(Enum):
    FIRE = "fire"
    WATER = "water"
    GRASS = "grass"

class MutableMonster:
    def __init__(self, name, hp, element):
        self.name, self.hp, self.element = name, hp, element

slime = MutableMonster("Slime", 30, Element.WATER)
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def hit_monster(atk, monster):
    monster.hp = max(0, monster.hp - atk)
# ANCHOR_END: procedural
hit_monster(10, slime)
assert slime.hp == 20
hit_monster(100, slime)
assert slime.hp == 0
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
from dataclasses import dataclass, replace

@dataclass(frozen=True)
class Monster:
    name: str
    hp: int
    element: Element

def hit_monster(atk, monster):
    return replace(monster, hp=max(0, monster.hp - atk))
# ANCHOR_END: functional
slime = Monster("Slime", 30, Element.WATER)
hit = hit_monster(10, slime)
assert hit.hp == 20 and slime.hp == 30 and hit.name == "Slime"
