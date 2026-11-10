# Testing a program that talks to the outside world, for the chapter
# "What IO really is".
# 外の世界と話すプログラムを試す。「IO の正体」の章
import io
import os
import sys


# ANCHOR: procedural
# Python (procedural style: swap sys.stdin and sys.stdout for a while)
# Python（手続き型スタイル：sys.stdin と sys.stdout を、しばらく差しかえる）
def greet():
    name = os.environ.get("USER", "stranger")
    print(f"Welcome, {name}!")
    quest = input("Which quest? ")
    print(f"Good luck with {quest}.")


def run_with(typed):
    saved = sys.stdin, sys.stdout
    sys.stdin, sys.stdout = io.StringIO(typed), io.StringIO()
    try:
        greet()
        return sys.stdout.getvalue()
    finally:
        sys.stdin, sys.stdout = saved
# ANCHOR_END: procedural


os.environ["USER"] = "Lambda"
assert run_with("slay the slime\n") == (
    "Welcome, Lambda!\nWhich quest? Good luck with slay the slime.\n")


# ANCHOR: functional
# Python (functional style: take the world, return a result and a new one)
# Python（関数型スタイル：世界を受け取り、結果と新しい世界を返す）
def put_str(text, world):
    return None, {**world, "screen": world["screen"] + text}


def get_line(world):
    line, *rest = world["stdin"]
    return line, {**world, "stdin": rest}


def greet_pure(world):
    name = world["env"].get("USER", "stranger")
    _, world = put_str(f"Welcome, {name}!\n", world)
    _, world = put_str("Which quest? ", world)
    quest, world = get_line(world)
    _, world = put_str(f"Good luck with {quest}.\n", world)
    return None, world


start = {"env": {"USER": "Lambda"}, "stdin": ["slay the slime"], "screen": ""}
_, end = greet_pure(start)
assert end["stdin"] == []
assert end["screen"].endswith("Good luck with slay the slime.\n")
assert start["stdin"] == ["slay the slime"]
# ANCHOR_END: functional
print("ok")
