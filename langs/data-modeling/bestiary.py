# The Python code in the data-modeling chapter.
# データをモデリングする章の Python のコード
# ANCHOR: with_enum
# Python (with Enum)
# Python（Enum を使う）
from enum import Enum

class Element(Enum):
    FIRE = "fire"
    WATER = "water"
    GRASS = "grass"
# ANCHOR_END: with_enum
assert Element.FIRE.value == "fire" and Element("water") is Element.WATER
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
class Attack:
    def __init__(self, power):
        self.power = power

class Defend:
    pass

def describe(action):
    if isinstance(action, Attack):
        return f"Attack with power {action.power}"
    elif isinstance(action, Defend):
        return "Defend"
# ANCHOR_END: procedural
assert describe(Attack(30)) == "Attack with power 30" and describe(Defend()) == "Defend"
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
from dataclasses import dataclass

@dataclass(frozen=True)
class Attack:
    power: int

@dataclass(frozen=True)
class Defend:
    pass

def describe(action):
    match action:
        case Attack(power):
            return f"Attack with power {power}"
        case Defend():
            return "Defend"
# ANCHOR_END: functional
assert describe(Attack(30)) == "Attack with power 30" and describe(Defend()) == "Defend"
