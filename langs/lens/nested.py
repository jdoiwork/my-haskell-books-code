# Changing a nested value, for the chapter "lens".
# 入れ子の奥の値を変える。「lens」の章
from dataclasses import dataclass, replace


@dataclass(frozen=True)
class Stats:
    hp: int
    mp: int
    level: int


@dataclass(frozen=True)
class Hero:
    name: str
    stats: Stats


@dataclass(frozen=True)
class Party:
    leader: Hero
    members: list


party = Party(Hero("Lambda", Stats(30, 10, 5)),
              [Hero("Python", Stats(25, 20, 4)),
               Hero("Rust", Stats(40, 5, 6))])

# ANCHOR: procedural
# Python (procedural style: change it in place, with a mutable copy)
# Python（手続き型スタイル：書き換えられる写しを作って、その場で変える）
import copy

original = {"leader": {"name": "Lambda",
                       "stats": {"hp": 30, "mp": 10, "level": 5}}}
data = copy.deepcopy(original)
data["leader"]["stats"]["hp"] = 0
# ANCHOR_END: procedural

# ANCHOR: replace
# Python (functional style: replace, one level at a time)
# Python（関数型スタイル：replace で、1段ずつ作り直す）
knocked_out = replace(
    party,
    leader=replace(party.leader,
                   stats=replace(party.leader.stats, hp=0)))
# ANCHOR_END: replace

assert data["leader"]["stats"]["hp"] == 0
assert original["leader"]["stats"]["hp"] == 30
assert knocked_out.leader.stats.hp == 0
assert party.leader.stats.hp == 30
print("ok")
