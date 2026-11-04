# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
import sys
from collections import Counter

text = sys.stdin.read()
words = "".join(c if c.isalpha() else " " for c in text).lower().split()
for w, n in Counter(words).most_common(10):
    print(f"{w:<12}{n}")
# ANCHOR_END: functional
