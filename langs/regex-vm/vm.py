# The Python code in the chapter on the VM of MiniRegex: the program
# that the re module compiles a pattern into, and a search with no
# limit on its steps.
# MiniRegex の命令の列の章の Python のコード：re モジュールがパターンから
# 作るプログラムと、ステップ数に上限のない探しもの
import contextlib
import io
import re
import time

shown = io.StringIO()
with contextlib.redirect_stdout(shown):
    # ANCHOR: program
    # Python: re.DEBUG prints the tree, and then the program
    # Python：re.DEBUG は、木を見せたあと、プログラムも見せる
    re.compile(r"colou?r|gr(a|e)y", re.DEBUG)
    # ANCHOR_END: program
program = shown.getvalue().split("\n\n")[1]
print(program, end="")
assert "BRANCH" in program and "MARK 0" in program and "JUMP" in program


# ANCHOR: nolimit
# Python: no limit on the steps. One more name, and the time is
#   four times as long
# Python：ステップ数に上限はない。名前が1つ増えると、時間は4倍
names = ["Mira", "Ruby", "Lam", "GHC", "Perl"] * 2
times = []
for n in [7, 8, 9]:
    field = " ".join(names[:n]) + "!"
    start = time.perf_counter()
    re.match(r"^(\w+\s?)+$", field)
    times.append(time.perf_counter() - start)
    print(n, f"{times[-1]:.2f}s")
# ANCHOR_END: nolimit

assert times[2] > 4 * times[0]
