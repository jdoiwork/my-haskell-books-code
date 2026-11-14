# The Python code in the search engine architecture chapter.
# 検索エンジンの設計を磨く章の Python のコード
import re
from dataclasses import dataclass
from itertools import groupby
from typing import Callable

STOP_WORDS = {"a", "an", "and", "the", "of", "on", "in", "to",
              "is", "it", "its", "at", "with", "for"}

def tokenize_en(text):
    pieces = re.split(r"[^0-9a-z]", text.lower())
    return [w for w in pieces if w and w not in STOP_WORDS]

# ANCHOR: bigram
# Python (bigrams: group the characters by kind, then cut)
# Python（bigram：文字を種類ごとにまとめてから、切る）
def kind(c):
    if "ぁ" <= c <= "ヿ" or "一" <= c <= "鿿":
        return "ja"
    return "alnum" if c.isalnum() else "other"

def tokenize_ja(text):
    tokens = []
    for k, group in groupby(text.lower(), key=kind):
        run = "".join(group)
        if k == "ja":
            if len(run) == 1:
                tokens.append(run)
            else:
                tokens += [run[i:i + 2] for i in range(len(run) - 1)]
        elif k == "alnum":
            tokens.append(run)
    return tokens
# ANCHOR_END: bigram

def excerpt_en(query, body):
    # The words around the first hit, the hits in [ ].
    # 最初に当たった語のまわり。当たった語は [ ] で囲む
    words = body.split()
    hits = set(tokenize_en(query))
    first = next((i for i, w in enumerate(words)
                  if set(tokenize_en(w)) & hits), 0)
    part = words[max(0, first - 5):first + 6]
    return " ".join(f"[{w}]" if set(tokenize_en(w)) & hits else w
                    for w in part)

def excerpt_ja(query, body):
    # The characters around the first word found, the words in [ ].
    # 最初に見つかった語のまわりの文字。語は [ ] で囲む
    found = [body.find(w) for w in query.split() if w in body]
    start = max(0, min(found, default=0) - 15)
    part = body[start:start + 30]
    for w in sorted(set(query.split()), key=len, reverse=True):
        part = part.replace(w, f"[{w}]")
    return part

# ANCHOR: record
# Python (a frozen dataclass that holds functions)
# Python（関数を持つ、書き換えられない dataclass）
@dataclass(frozen=True)
class Tokenizer:
    name: str
    cut: Callable[[str], list[str]]
    excerpt_of: Callable[[str, str], str]  # query, body -> excerpt

ENGLISH = Tokenizer("en", tokenize_en, excerpt_en)
JAPANESE = Tokenizer("ja", tokenize_ja, excerpt_ja)
TOKENIZERS = {t.name: t for t in [ENGLISH, JAPANESE]}
# ANCHOR_END: record

assert tokenize_ja("赤いドラゴン") == ["赤い", "いド", "ドラ", "ラゴ", "ゴン"]
assert tokenize_ja("HP 30のスライム") == [
    "hp", "30", "のス", "スラ", "ライ", "イム"]
assert TOKENIZERS["en"].cut("The red Dragon, of fire!") == [
    "red", "dragon", "fire"]
assert tokenize_ja("") == []
assert tokenize_ja("炎") == ["炎"]
assert ENGLISH.excerpt_of("dragon", "The red dragon sleeps") == (
    "The red [dragon] sleeps")
assert JAPANESE.excerpt_of("ドラゴン", "赤いドラゴン") == "赤い[ドラゴン]"
print("ok")
