# Make pcre2/clusters.in: short lines of the characters that \X's rules
# look at, mixed (the seed is 618, so it is the same each time).
# pcre2/clusters.in を作る：\X の規則が見る文字を混ぜた、短い行
# （種は 618 なので、毎回同じ）
import random

POOL = [
    "a", "1", "#", " ", "\r", "\n", "\t", "é",
    "́", "̈", "‍", "‌", "️", "⃣",  # marks
    "\U0001f1ef", "\U0001f1f5", "\U0001f1fa", "\U0001f1f8",      # 🇯 🇵 🇺 🇸
    "\U0001f468", "\U0001f469", "\U0001f467", "\U0001f44d",      # 👨 👩 👧 👍
    "\U0001f3fd", "❤", "\U0001f525", "©",              # 🏽 ❤ 🔥 ©
    "ᄀ", "ᅡ", "ᆨ", "가", "각",            # Hangul
    "क", "्", "ष", "़", "ि",            # Devanagari
    "؀", "ः", "ำ", "\u0001", "­",            # Prepend ...
]

random.seed(618)
with open("pcre2/clusters.in", "w", encoding="utf-8", newline="\n") as out:
    for _ in range(3000):
        line = "".join(random.choice(POOL) for _ in range(random.randint(1, 10)))
        # a line of the file cannot hold \n itself: write it as \r only
        # 行の中に \n そのものは書けないので、\r にする
        out.write(line.replace("\n", "\r") + "\n")
