# The Python code in the chapter on regular expressions: hide the phone
# numbers, leaving the last 4 digits. First a loop over the matches,
# then re.sub with a function.
# 正規表現の章の Python のコード：電話番号を、下の4桁だけ残して伏せる。
# まずは見つかったものをループで、次に関数を渡す re.sub で
import re
import time

text = "call 080-9876-5432 or 03-1234-5678 now"
want = "call ***-****-5432 or ***-****-5678 now"

# ANCHOR: procedural
# Python (procedural style): walk over the matches, and glue the pieces
# Python（手続き型スタイル）：見つかったものを順に見て、切れ端をつなぐ
phone = re.compile(r"\d{2,4}-\d{2,4}-(?P<last>\d{4})")


def hide(text):
    out = ""
    end = 0
    for m in phone.finditer(text):
        out += text[end:m.start()]
        out += "***-****-" + m.group("last")
        end = m.end()
    out += text[end:]
    return out
# ANCHOR_END: procedural


assert hide(text) == want


# ANCHOR: functional
# Python (functional style): re.sub takes a function, too
# Python（関数型スタイル）：re.sub は関数も受け取る
def hide_sub(text):
    return phone.sub(lambda m: f"***-****-{m['last']}", text)
# ANCHOR_END: functional


assert hide_sub(text) == want

# The replacement string has a small language of its own: \g<last>.
# 置き換えの文字列には、小さな言語がある
assert phone.sub(r"***-****-\g<last>", text) == want


# ANCHOR: lateError
# Python: a mistake in a pattern is found when re.compile runs
# Python：パターンのまちがいは、re.compile が動いたときに見つかる
def broken():
    return re.compile(r"(\d{4}")


print("defined broken")
try:
    broken()
except re.error as e:
    print("re.error:", e)
# ANCHOR_END: lateError

    assert str(e) == "missing ), unterminated subpattern at position 0"

# Backtracking: each a more doubles the time (no limit in re).
# バックトラック：a を1つ増やすごとに、時間が倍になる（re に上限はない）
times = []
for n in [18, 19, 20]:
    start = time.perf_counter()
    assert re.match(r"(a+)+$", "a" * n + "b") is None
    times.append(time.perf_counter() - start)
assert times[2] > times[0] * 2
