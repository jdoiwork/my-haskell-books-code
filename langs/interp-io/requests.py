# The Python code in the chapter on IO: a program that asks for things,
# and a runner that does them (with a generator).
# 入出力の章の Python のコード：頼みごとをするプログラムと、それをやる
# 実行役（ジェネレーターで）
import io
import contextlib


# ANCHOR: procedural
# Python (procedural style): the function does IO itself
# Python（手続き型スタイル）：関数が、自分で IO をする
def greet():
    name = input("What is your name? ")
    print(f"Hello, {name}!")
# ANCHOR_END: procedural


# To test it, we must swap the real stdin and stdout.
# テストするには、本物の stdin と stdout を差し替えなければならない
def test_greet():
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        import sys
        real_stdin = sys.stdin
        sys.stdin = io.StringIO("Lam\n")
        try:
            greet()
        finally:
            sys.stdin = real_stdin
    assert out.getvalue() == "What is your name? Hello, Lam!\n"


test_greet()


# ANCHOR: requests
# Python (generator style): yield a request, and get the answer back.
# The function itself never touches the outside world.
# Python（ジェネレーターのスタイル）：頼みごとを yield し、答えを受け取る。
# 関数そのものは、外の世界に触れない
def greet_requests():
    yield ("put", "What is your name? ")
    name = yield ("get_line",)
    yield ("put", f"Hello, {name}!\n")


# The runner: send each answer back, until the generator ends.
# 実行役：ジェネレーターが終わるまで、答えを送り返す
def run(program, handle):
    gen = program()
    answer = None
    try:
        while True:
            request = gen.send(answer)
            answer = handle(request)
    except StopIteration:
        pass
# ANCHOR_END: requests


# ANCHOR: handlers
# The real world, and a fake one made of a list and a string.
# 本物の世界と、リストと文字列でできた偽物の世界
def real_world(request):
    if request[0] == "put":
        print(request[1], end="", flush=True)
    elif request[0] == "get_line":
        return input()


def fake_world(lines):
    screen = []

    def handle(request):
        if request[0] == "put":
            screen.append(request[1])
        elif request[0] == "get_line":
            return lines.pop(0)
    return handle, screen


handle, screen = fake_world(["Lam"])
run(greet_requests, handle)
print("".join(screen), end="")
# What is your name? Hello, Lam!
# ANCHOR_END: handlers

assert "".join(screen) == "What is your name? Hello, Lam!\n"

# The real world works too (with stdin swapped, only for this check).
# 本物の世界でも動く（ここで確かめるためだけに、stdin を差し替える）
out = io.StringIO()
with contextlib.redirect_stdout(out):
    import sys
    real_stdin = sys.stdin
    sys.stdin = io.StringIO("Lam\n")
    try:
        run(greet_requests, real_world)
    finally:
        sys.stdin = real_stdin
assert out.getvalue() == "What is your name? Hello, Lam!\n"
