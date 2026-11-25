# The Python code in the chapter on the cursor and the instructions:
# what re.DEBUG shows for repeats. A repeat of one character becomes
# REPEAT_ONE; a longer body becomes REPEAT ... MAX_UNTIL.
# しおりと命令を磨く章の Python のコード：くり返しに re.DEBUG が見せる
# もの。1文字のくり返しは REPEAT_ONE に、もっと長い中身は
# REPEAT … MAX_UNTIL になる
# ANCHOR: helper
import contextlib
import io
import re


# re.DEBUG prints the parsed tree and then the program; keep the program
# re.DEBUG は、読んだ木とプログラムを書き出す。プログラムだけを取り出す
def program(pattern):
    shown = io.StringIO()
    with contextlib.redirect_stdout(shown):
        re.compile(pattern, re.DEBUG)
    # the second half is the program; the first is the parsed tree
    # 後半がプログラム。前半は読んだ木
    return [line.rstrip() for line in
            shown.getvalue().split("\n\n")[1].splitlines()]
# ANCHOR_END: helper


# ANCHOR: repeat
# Python: the programs of three repeats
# Python：3つのくり返しのプログラム
for pattern in [r"a*b", r"(?:ab)*c", r"(?:a|)*b"]:
    print(pattern)
    for line in program(pattern):
        print("   ", line)
# ANCHOR_END: repeat

a_star = program(r"a*b")
assert any("REPEAT_ONE" in line for line in a_star)
assert not any("MAX_UNTIL" in line for line in a_star)
ab_star = program(r"(?:ab)*c")
assert any("REPEAT " in line for line in ab_star)
assert any("MAX_UNTIL" in line for line in ab_star)
empty = program(r"(?:a|)*b")
assert any("MAX_UNTIL" in line for line in empty)
