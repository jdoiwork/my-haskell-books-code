# The outcome of a quest, for the chapter "ADTs are everywhere".
# 依頼の結果。「代数的データ型は、どこにでもある」の章
from dataclasses import dataclass
from typing import assert_never


# ANCHOR: procedural
# Python (procedural style: one class, a status string, optional fields)
# Python（手続き型スタイル：1つのクラス、状態の文字列、省略できるフィールド）
class QuestOutcome:
    def __init__(self, status, gold=None, place=None):
        self.status = status
        self.gold = gold
        self.place = place


def describe_old(outcome):
    if outcome.status == "cleared":
        return f"cleared! +{outcome.gold} G"
    elif outcome.status == "failed":
        return f"failed at {outcome.place}"
    else:
        return "ran away"
# ANCHOR_END: procedural


assert describe_old(QuestOutcome("cleared", gold=50)) == "cleared! +50 G"
# A typo in the status falls through to "ran away" silently.
# 状態の打ちまちがいは、だまって "ran away" に流れる
assert describe_old(QuestOutcome("claered", gold=50)) == "ran away"


# ANCHOR: functional
# Python (functional style: one dataclass per case, match, assert_never)
# Python（関数型スタイル：場合ごとの dataclass、match、assert_never）
@dataclass(frozen=True)
class Cleared:
    gold: int


@dataclass(frozen=True)
class Failed:
    place: str


@dataclass(frozen=True)
class Fled:
    pass


type Outcome = Cleared | Failed | Fled


def describe(outcome: Outcome) -> str:
    match outcome:
        case Cleared(gold):
            return f"cleared! +{gold} G"
        case Failed(place):
            return f"failed at {place}"
        case Fled():
            return "ran away"
        case _:
            assert_never(outcome)
# ANCHOR_END: functional


assert describe(Cleared(50)) == "cleared! +50 G"
assert describe(Failed("cave")) == "failed at cave"
assert describe(Fled()) == "ran away"
print("ok")
