# Switching a language feature on for one file, for the chapter
# "GHC extensions".
# ファイルごとに、言語の機能を有効にする。「GHC 拡張」の章

# ANCHOR: future
# Python (one file opts in to a feature, like a LANGUAGE pragma)
# Python（1つのファイルだけ、機能を有効にする。LANGUAGE プラグマのように）
from __future__ import annotations


class Hero:
    name: str
    party: Party      # Party is defined below; fine with the import


class Party:
    leader: Hero
# ANCHOR_END: future

# With the import, annotations stay as strings.
# この import があると、型の注釈は文字列のまま残る
assert Hero.__annotations__["party"] == "Party"
print("ok")
