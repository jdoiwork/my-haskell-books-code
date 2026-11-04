# Effects as requests, for the chapter "Effects".
# 頼みごととしてのエフェクト。「エフェクト」の章
import random
from dataclasses import dataclass


# ANCHOR: procedural
# Python (procedural style: roll and print right where we need them)
# Python（手続き型スタイル：必要な場所で、その場で振って表示する）
def attack_now():
    hit = random.randint(1, 20)
    if hit >= 10:
        damage = random.randint(1, 6)
        print(f"hit for {damage}")
        return damage
    print("missed")
    return 0
# ANCHOR_END: procedural


random.seed(1)
assert 0 <= attack_now() <= 6


# ANCHOR: functional
# Python (generators: the program yields requests, a handler answers)
# Python（ジェネレーター：頼みごとを yield し、ハンドラーが答える）
@dataclass(frozen=True)
class Roll:
    sides: int


@dataclass(frozen=True)
class Say:
    text: str


def attack():
    hit = yield Roll(20)
    if hit >= 10:
        damage = yield Roll(6)
        yield Say(f"hit for {damage}")
        return damage
    yield Say("missed")
    return 0


def battle():
    total = 0
    for _ in range(3):
        total += yield from attack()
    return total


def run_scripted(rolls, program):
    said, rolls = [], iter(rolls)
    gen = program()
    try:
        request = next(gen)
        while True:
            match request:
                case Roll(sides):
                    request = gen.send(1 + (next(rolls, 1) - 1) % sides)
                case Say(text):
                    said.append(text)
                    request = gen.send(None)
    except StopIteration as finished:
        return said, finished.value
# ANCHOR_END: functional


assert run_scripted([15, 4, 3, 12, 6], battle) == (
    ["hit for 4", "missed", "hit for 6"], 10)
assert run_scripted([10, -1], battle)[1] == 5
print("ok")
