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

# ANCHOR: record
# Python (a frozen dataclass that holds functions)
# Python（関数を持つ、書き換えられない dataclass）
@dataclass(frozen=True)
class Tokenizer:
    name: str
    cut: Callable[[str], list[str]]

ENGLISH = Tokenizer("en", tokenize_en)
JAPANESE = Tokenizer("ja", tokenize_ja)
TOKENIZERS = {t.name: t for t in [ENGLISH, JAPANESE]}
# ANCHOR_END: record

assert tokenize_ja("赤いドラゴン") == ["赤い", "いド", "ドラ", "ラゴ", "ゴン"]
assert tokenize_ja("HP 30のスライム") == [
    "hp", "30", "のス", "スラ", "ライ", "イム"]
assert TOKENIZERS["en"].cut("The red Dragon, of fire!") == [
    "red", "dragon", "fire"]
print("ok")
