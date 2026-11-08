# The bookshelf's types in Python, for the chapter
# "Design: works, contents, notes and ratings as types".
# 「設計：作品・目次・感想・評価を型で描く」の章の、本棚の型を Python で

# ANCHOR: procedural
# Python (procedural style: plain numbers, strings and a dict)
# Python（手続き型スタイル：ただの数と文字列と dict）

# ⭐ Check the stars: 1 to 5, by halves. Keep them as half stars.
# ★を確かめる：1〜5 で、0.5刻み。半分の★の数で持つ
def make_stars(x):
    if x < 1 or x > 5 or x * 2 != int(x * 2):
        raise ValueError(f"bad stars: {x}")
    return int(x * 2)


# 🔁 A score to stars: round halves up, never below 1 star.
# 点を★に：真ん中は切り上げ。★1 より下にはしない
def score_to_stars(n):
    return min(max((n + 5) // 10, 2), 10)


# 🗺️ The map of reading: (state, event) -> next state.
# 読書の地図：(状態, できごと) → 次の状態
MOVES = {
    ("piled", "start"): "reading",
    ("reading", "finish"): "finished",
    ("reading", "stop"): "paused",
    ("paused", "resume"): "reading",
    ("finished", "reread"): "rereading",
    ("rereading", "finish"): "finished",
    ("rereading", "stop"): "finished",
}


def advance(event, state):
    if (state, event) not in MOVES:
        raise ValueError(f"not allowed: {event} from {state}")
    return MOVES[(state, event)]
# ANCHOR_END: procedural


assert make_stars(3.5) == 7
for bad in [6, 3.3, -3]:
    try:
        make_stars(bad)
        assert False, bad
    except ValueError:
        pass
assert score_to_stars(25) == 3
assert score_to_stars(24) == 2
assert all(score_to_stars(h * 10) == h for h in range(2, 11))
assert advance("start", "piled") == "reading"
assert advance("stop", "rereading") == "finished"
try:
    advance("start", "finished")
    assert False
except ValueError as e:
    assert str(e) == "not allowed: start from finished"


# ANCHOR: functional
# Python (functional style: a frozen dataclass that checks itself)
# Python（関数型スタイル：自分を確かめる、変えられない dataclass）
from dataclasses import dataclass


# ⭐ Stars kept as half stars (2 to 10). Checked when made.
# ★は半分の★の数（2〜10）で持つ。作るときに確かめる
@dataclass(frozen=True)
class Stars:
    halves: int

    def __post_init__(self):
        if not 2 <= self.halves <= 10:
            raise ValueError(f"bad stars: {self.halves / 2}")

    @staticmethod
    def of(x: float) -> "Stars":
        if x * 2 != int(x * 2):
            raise ValueError(f"bad stars: {x}")
        return Stars(int(x * 2))
# ANCHOR_END: functional


@dataclass(frozen=True)
class Score:
    value: int

    def __post_init__(self):
        if not 0 <= self.value <= 100:
            raise ValueError(f"bad score: {self.value}")


def stars_to_score(s: Stars) -> Score:
    return Score(s.halves * 10)


def to_stars(p: Score) -> Stars:
    return Stars(min(max((p.value + 5) // 10, 2), 10))


# ANCHOR: states
# Python (functional style: Enum keys in the map of reading)
# Python（関数型スタイル：読書の地図のキーに Enum）
from enum import Enum

State = Enum("State", "PILED READING FINISHED PAUSED REREADING")
Event = Enum("Event", "START FINISH STOP RESUME REREAD")
S, E = State, Event
MOVES = {
    (S.PILED, E.START): S.READING,
    (S.READING, E.FINISH): S.FINISHED,
    (S.READING, E.STOP): S.PAUSED,
    (S.PAUSED, E.RESUME): S.READING,
    (S.FINISHED, E.REREAD): S.REREADING,
    (S.REREADING, E.FINISH): S.FINISHED,
    (S.REREADING, E.STOP): S.FINISHED,
}


# ➡️ Take one step, or say why not.
# 1歩進む。進めなければ、そのわけを言う
def advance(event: Event, state: State) -> State:
    if (state, event) not in MOVES:
        raise ValueError(f"not allowed: {event.name} from {state.name}")
    return MOVES[(state, event)]
# ANCHOR_END: states


assert Stars.of(3.5) == Stars(7)
for bad in [6, 3.3, -3]:
    try:
        Stars.of(bad)
        assert False, bad
    except ValueError:
        pass
for bad in [-3, 101]:
    try:
        Score(bad)
        assert False, bad
    except ValueError:
        pass
assert to_stars(Score(25)) == Stars(3)
assert to_stars(Score(24)) == Stars(2)
everyone = [Stars(h) for h in range(2, 11)]
assert [to_stars(stars_to_score(s)) for s in everyone] == everyone
assert advance(E.STOP, S.REREADING) == S.FINISHED
try:
    advance(E.START, S.FINISHED)
    assert False
except ValueError:
    pass


# ANCHOR: kinds
# Python (match over the kinds of works)
# Python（作品の種類で match）
@dataclass(frozen=True)
class Book:
    toc: list[tuple[str, int]]  # (heading, pages) / (見出し, ページ)


@dataclass(frozen=True)
class Film:
    minutes: int


@dataclass(frozen=True)
class Game:
    pass


# 📏 How long is it? Each kind answers in its own unit.
# どれくらいの長さ？種類ごとに、自分の単位で答える
def length(work: Book | Film | Game) -> str:
    match work:
        case Book(toc):
            return f"{sum(p for _, p in toc)} pages"
        case Film(minutes):
            return f"{minutes} minutes"
        case Game():
            return "no table of contents"
# ANCHOR_END: kinds


dragon_book = Book([("第1部 ドラゴンを知る", 120), ("第2部 ドラゴンと話す", 200)])
last_dragon = Film(121)
assert length(dragon_book) == "320 pages"
assert length(last_dragon) == "121 minutes"
assert length(Game()) == "no table of contents"
