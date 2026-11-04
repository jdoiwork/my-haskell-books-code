# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
import sys

text = sys.stdin.read()

# Replace non-letters with spaces and make everything lowercase.
# 英字以外を空白に置き換え、小文字にする
letters_only = ""
for c in text:
    if c.isalpha():
        letters_only += c
    else:
        letters_only += " "
words = letters_only.lower().split()

# Count how many times each word appears.
# 単語ごとの出現回数を数える
counts = {}
for w in words:
    if w in counts:
        counts[w] += 1
    else:
        counts[w] = 1

# Sort by count (descending) and print the top 10.
# 回数の多い順に並べ、上位10件を表示する
ranking = sorted(counts.items(), key=lambda pair: pair[1], reverse=True)
for w, n in ranking[:10]:
    print(f"{w:<12}{n}")
# ANCHOR_END: procedural
