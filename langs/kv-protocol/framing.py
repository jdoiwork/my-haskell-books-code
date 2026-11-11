# Reading length-prefixed strings in Python, for the chapter
# "Protocol and parser".
# 「プロトコルとパーサー」の章の、長さが先に付いた文字列の読み方を Python で


# ANCHOR: procedural
# Python (procedural style: read the length first, wait if short)
# Python（手続き型スタイル：先に長さを読み、足りなければ待つ）
def parse_bulk(buffer):
    if buffer and not buffer.startswith(b"$"):
        raise ValueError("not a bulk string")   # broken / 壊れている
    end = buffer.find(b"\r\n")
    if end < 0:
        return None                        # need more / 続きを待つ
    length = int(buffer[1:end])
    start = end + 2
    if len(buffer) < start + length + 2:
        return None                        # need more / 続きを待つ
    return buffer[start:start + length], buffer[start + length + 2:]
# ANCHOR_END: procedural

assert parse_bulk(b"$6\r\nLambda\r\n:42\r\n") == (b"Lambda", b":42\r\n")
assert parse_bulk(b"$6\r\nLam") is None
try:
    parse_bulk(b"+OK\r\n")
    raise AssertionError("should be broken")
except ValueError:
    pass

# ANCHOR: typed
# Python (functional style: three results as types, checked by match)
# Python（関数型スタイル：3つの結果を型にして、match で場合分け）
from dataclasses import dataclass
from typing import assert_never


@dataclass
class NeedMore:
    pass


@dataclass
class Broken:
    why: str


@dataclass
class Parsed:
    value: bytes
    rest: bytes


def parse_bulk_typed(buffer: bytes) -> NeedMore | Broken | Parsed:
    if not buffer:
        return NeedMore()
    if not buffer.startswith(b"$"):
        return Broken("not a bulk string")
    end = buffer.find(b"\r\n")
    if end < 0:
        return NeedMore()
    length, start = int(buffer[1:end]), end + 2
    if len(buffer) < start + length + 2:
        return NeedMore()
    return Parsed(buffer[start:start + length], buffer[start + length + 2:])


def describe(result: NeedMore | Broken | Parsed) -> str:
    match result:
        case NeedMore():
            return "wait"
        case Broken(why):
            return "broken: " + why
        case Parsed(value, _):
            return "got " + value.decode()
        case _:
            assert_never(result)   # mypy and pyright check all three
# ANCHOR_END: typed

assert describe(parse_bulk_typed(b"$6\r\nLambda\r\n")) == "got Lambda"
assert describe(parse_bulk_typed(b"$6\r\nLam")) == "wait"
assert describe(parse_bulk_typed(b"+OK\r\n")) == "broken: not a bulk string"

# ANCHOR: naive
# Python (splitting on lines breaks a value that contains \r\n)
# Python（行で区切ると、\r\n を含む値が壊れる）
message = b"SET note first-line\r\nsecond-line\r\n"
words = message.split(b"\r\n")[0].split()   # the value is cut in two
# ANCHOR_END: naive
assert words == [b"SET", b"note", b"first-line"]
assert parse_bulk(b"$23\r\nfirst-line\r\nsecond-line\r\n") == \
    (b"first-line\r\nsecond-line", b"")
print("ok")
