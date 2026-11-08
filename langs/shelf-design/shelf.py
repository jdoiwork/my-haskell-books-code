# The bookshelf's types in Python, for the chapter
# "Design: works, contents, notes and ratings as types".
# 「設計：作品・目次・感想・評価を型で描く」の章の、本棚の型を Python で

# ANCHOR: procedural
# Python (procedural style: a plain number and style names as strings)
# Python（手続き型スタイル：ただの数と、文字列の見せ方の名前）

# 💯 Check a score: a whole number from 0 to 100.
# 点を確かめる：0〜100 の整数
def make_score(n):
    if n < 0 or n > 100:
        raise ValueError(f"bad score: {n}")
    return n


# 🖼️ Show a score: "hundred", "stars", "ten" or "ten1" (7.3/10).
# 点を見せる："hundred"、"stars"、"ten"、"ten1"（7.3/10）
def render(score, style):
    if style == "hundred":
        return f"{score}点"
    elif style == "stars":
        halves = (score + 5) // 10        # halves go up / 真ん中は切り上げ
        if halves % 2 == 0:
            return f"★{halves // 2}"
        else:
            return f"★{halves // 2}.5"
    elif style == "ten":
        return f"{(score + 5) // 10}/10"
    elif style == "ten1":
        return f"{score // 10}.{score % 10}/10"
    else:
        raise ValueError(f"bad style: {style}")
# ANCHOR_END: procedural


# ANCHOR: moves
# Python (procedural style: the map of reading as a dict)
# Python（手続き型スタイル：読書の地図を dict で）

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
# ANCHOR_END: moves


def refused(f):
    try:
        f()
        return False
    except ValueError:
        return True


assert make_score(73) == 73
assert all(refused(lambda: make_score(n)) for n in [-1, 101])
assert [render(73, s) for s in ["hundred", "stars", "ten", "ten1"]] == [
    "73点", "★3.5", "7/10", "7.3/10"]
assert render(75, "stars") == "★4"
assert render(25, "stars") == "★1.5"
assert render(4, "stars") == "★0"
assert render(75, "ten") == "8/10"
assert render(80, "ten1") == "8.0/10"
assert refused(lambda: render(73, "starz"))
# The language's round() rounds halves to even: not what we want.
# 言語の round() は、真ん中を偶数に丸める。ここで欲しいものではない
assert round(2.5) == 2 and round(7.5) == 8
assert advance("start", "piled") == "reading"
assert advance("stop", "rereading") == "finished"
try:
    advance("start", "finished")
    assert False
except ValueError as e:
    assert str(e) == "not allowed: start from finished"


# ANCHOR: functional
# Python (functional style: frozen dataclasses for the model and styles)
# Python（関数型スタイル：モデルと見せ方を、変えられない dataclass で）
from dataclasses import dataclass


# 💯 The model: a score from 0 to 100. Checked when made.
# モデル：0〜100 の点。作るときに確かめる
@dataclass(frozen=True)
class Score:
    points: int

    def __post_init__(self):
        if not 0 <= self.points <= 100:
            raise ValueError(f"bad score: {self.points}")


# 🎨 Ways to show a score. None of them is the score itself.
# 点の見せ方。どれも、点そのものではない
@dataclass(frozen=True)
class Hundred:          # 73点
    pass


@dataclass(frozen=True)
class FiveStars:        # ★3.5
    pass


@dataclass(frozen=True)
class OutOfTen:         # 7/10, 7.3/10
    one_decimal: bool


Style = Hundred | FiveStars | OutOfTen
# ANCHOR_END: functional

ALL_STYLES: list[Style] = [
    Hundred(), FiveStars(), OutOfTen(False), OutOfTen(True)]


# ANCHOR: render
# Python (functional style: render with match)
# Python（関数型スタイル：match で見せる）
from typing import assert_never


# 📏 How many points one step of the style is worth.
# その見せ方の1目盛りが、何点ぶんか
def step(style: Style) -> int:
    match style:
        case Hundred() | OutOfTen(True):
            return 1
        case FiveStars() | OutOfTen(False):
            return 10
        case _:
            assert_never(style)


# 🖼️ Show a score; a coarse style rounds halves up (75 is ★4).
# 点を見せる。粗い見せ方は、真ん中を切り上げる（75 点は★4）
def render(style: Style, score: Score) -> str:
    n = score.points
    steps = (n + step(style) // 2) // step(style)
    match style:
        case Hundred():
            return f"{n}点"
        case FiveStars():
            return "★" + tenths(steps * 5)
        case OutOfTen(False):
            return f"{steps}/10"
        case OutOfTen(True):
            return f"{n // 10}.{n % 10}/10"
        case _:
            assert_never(style)


# 🔢 35 tenths is "3.5"; 40 is "4".
# 0.1 が 35 個なら "3.5"、40 個なら "4"
def tenths(t: int) -> str:
    whole, decimal = divmod(t, 10)
    return f"{whole}" if decimal == 0 else f"{whole}.{decimal}"
# ANCHOR_END: render


# ANCHOR: parse
# Python (functional style: read typed text back into the model)
# Python（関数型スタイル：打ちこまれた文字を、モデルに読み戻す）
import re


# 📥 Read text in a style back into a score. Marks may be left out.
# その見せ方の文字を、点に読み戻す。印は省いてもよい
def parse(style: Style, text: str) -> Score:
    t = read_tenths(drop_mark(style, text.strip()).strip())
    if t is not None:
        match style:
            case Hundred() if t % 10 == 0 and t <= 1000:
                return Score(t // 10)
            case FiveStars() if t % 5 == 0 and t <= 50:
                return Score(t * 2)
            case OutOfTen(False) if t % 10 == 0 and t <= 100:
                return Score(t)
            case OutOfTen(True) if t <= 100:
                return Score(t)
    raise ValueError(f"bad rating: {text}")


# ✂️ Take off the style's own mark, if it is there.
# その見せ方の印があれば、取る
def drop_mark(style: Style, text: str) -> str:
    match style:
        case Hundred():
            return text.removesuffix("点")
        case FiveStars():
            return text.removeprefix("★")
        case OutOfTen():
            return text.removesuffix("/10")


# 🔢 "7" is 70 tenths, "7.3" is 73; [0-9], since \d takes "７" too.
# "7" は 0.1 が 70 個、"7.3" は 73 個。\d は "７" も通すので [0-9]
def read_tenths(text: str) -> int | None:
    m = re.fullmatch(r"([0-9]{1,4})(?:\.([0-9]))?", text)
    if m is None:
        return None
    return int(m[1]) * 10 + int(m[2] or 0)
# ANCHOR_END: parse


assert Score(73).points == 73
assert all(refused(lambda: Score(n)) for n in [-1, 101])
assert [render(s, Score(73)) for s in ALL_STYLES] == [
    "73点", "★3.5", "7/10", "7.3/10"]
assert render(FiveStars(), Score(75)) == "★4"
assert render(FiveStars(), Score(25)) == "★1.5"
assert render(FiveStars(), Score(4)) == "★0"
assert render(FiveStars(), Score(100)) == "★5"
assert render(OutOfTen(False), Score(75)) == "8/10"
assert render(OutOfTen(True), Score(80)) == "8.0/10"
assert parse(FiveStars(), "★4.5") == Score(90)
assert parse(FiveStars(), " 4 ") == Score(80)
assert parse(Hundred(), "73点") == Score(73)
assert parse(OutOfTen(True), "7.5") == Score(75)
assert parse(OutOfTen(True), "8.0/10") == Score(80)
assert parse(OutOfTen(False), "7/10") == Score(70)
bad = [(FiveStars(), "★6"), (FiveStars(), "★3.3"),
       (OutOfTen(False), "7.5"), (OutOfTen(True), "10.1"),
       (Hundred(), "101"), (Hundred(), "7.5"), (Hundred(), "-3"),
       (Hundred(), "７３"), (FiveStars(), "3.25"), (Hundred(), "")]
for style, text in bad:
    try:
        parse(style, text)
        assert False, (style, text)
    except ValueError as e:
        assert str(e).startswith("bad rating"), e
for style in ALL_STYLES:
    for n in range(101):
        shown = parse(style, render(style, Score(n))).points
        # A shadow is near the model; on a step, it is the model itself.
        # 影はモデルの近く。目盛りの上なら、モデルそのもの
        assert abs(shown - n) <= step(style) // 2, (style, n)
        if n % step(style) == 0:
            assert shown == n, (style, n)


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
