# The Python code in the chapter on looking first: what re.DEBUG shows
# at the head of the program (INFO): a prefix with its overlap table
# (KMP's table), or a set of first characters.
# 先に探す章の Python のコード：re.DEBUG がプログラムの頭（INFO）に
# 見せるもの。先頭の文字列と、その overlap の表（KMP の表）か、最初の
# 文字の集合
import contextlib
import io
import re


def info(pattern):
    shown = io.StringIO()
    with contextlib.redirect_stdout(shown):
        re.compile(pattern, re.DEBUG)
    program = shown.getvalue().split("\n\n")[1].splitlines()
    # INFO covers the lines up to "(to N)"
    # INFO は「(to N)」の手前の行まで
    end = int(re.search(r"\(to (\d+)\)", program[0])[1])
    head = []
    for line in program[1:]:
        number = re.match(r"\s*(\d+)[.:]", line)
        if number and int(number[1]) >= end:
            break
        head.append(line.strip())
    return program[0], head


# ANCHOR: info
# Python: the INFO at the head of the program, before the matcher runs
# Python：プログラムの頭の INFO。マッチャーを動かす前に見るもの
for pattern in [r"Room (\d+)", r"abcabd", r"colou?r|grey"]:
    first, head = info(pattern)
    print(pattern)
    for line in head:
        print("   ", line)
# ANCHOR_END: info

_, room = info(r"Room (\d+)")
assert "prefix [0x52, 0x6f, 0x6f, 0x6d, 0x20] ('Room ')" in room
assert "overlap [0, 0, 0, 0, 0]" in room
_, abc = info(r"abcabd")
assert "overlap [0, 0, 0, 1, 2, 0]" in abc
_, colour = info(r"colou?r|grey")
assert colour[0] == "in"
