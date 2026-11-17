#!/usr/bin/env python3
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
import sys

# The file in the arguments, or the standard input.
# 引数のファイルか、標準入力
args = sys.argv[1:]
if len(args) == 0:
    text = sys.stdin.read()
elif len(args) == 1:
    try:
        with open(args[0], encoding="utf-8") as f:
            text = f.read()
    except OSError as e:
        print(f"wordfreq: {args[0]}: {e.strerror}", file=sys.stderr)
        sys.exit(1)
else:
    print("wordfreq: give one file, or none", file=sys.stderr)
    sys.exit(1)

# Count the words, letter by letter.
# 文字を1つずつ見て、単語を数える
counts = {}
word = ""
for c in text.lower() + " ":
    if c.isalpha():
        word += c
    elif word != "":
        counts[word] = counts.get(word, 0) + 1
        word = ""

# The most frequent first; the same count from a to z.
# 多い順。同じ回数なら a から z
ranking = sorted(counts.items(), key=lambda pair: (-pair[1], pair[0]))
for w, n in ranking[:10]:
    print(f"{w:<12}{n}")
# ANCHOR_END: procedural
