# The comparison code of the chapter "Tests and debugging: lam test".
# pytest finds the functions named test_*; capsys, monkeypatch and
# tmp_path are the fake world. Hypothesis does the properties.
# 「テストとデバッグ：lam test」の章の比較のコード。pytest は test_ で
# 始まる関数を探す。capsys・monkeypatch・tmp_path が偽物の世界。性質は
# Hypothesis で
import io
import re
import sys
from collections import Counter

from hypothesis import find, given, settings
from hypothesis import strategies as st


# ANCHOR: code
# Python (the code to test)
# Python（テストするコード）
def split_into_words(text):
    return re.findall(r"[a-z']+", text.lower())


def top_n(n, counts):
    return sorted(counts.items(), key=lambda p: (-p[1], p[0]))[:n]


def main(args):
    if args:
        try:
            with open(args[0]) as f:
                text = f.read()
        except FileNotFoundError:
            print(f"wordfreq: {args[0]}: no such file", file=sys.stderr)
            return 1
    else:
        text = sys.stdin.read()
    for word, count in top_n(3, Counter(split_into_words(text))):
        print(f"{word:<8}{count}")
    return 0
# ANCHOR_END: code


# ANCHOR: plain
# Python (plain asserts: pytest shows both sides when one fails)
# Python（ただの assert。落ちると、pytest が両側を見せる）
def test_makes_lowercase_words():
    assert split_into_words("Hello, World!") == ["hello", "world"]


def test_keeps_dont_as_one_word():
    assert split_into_words("Don't panic") == ["don't", "panic"]
# ANCHOR_END: plain


# ANCHOR: fakeWorld
# Python (the fake world: stdin, the screen, and a folder of our own)
# Python（偽物の世界：標準入力、画面、自分だけのフォルダー）
def test_reads_the_standard_input(monkeypatch, capsys):
    monkeypatch.setattr(sys, "stdin", io.StringIO("b a b\nc a b\n"))
    assert main([]) == 0
    assert capsys.readouterr().out == "b       3\na       2\nc       1\n"


def test_reads_the_file(tmp_path, capsys):
    notes = tmp_path / "notes.txt"
    notes.write_text("Don't panic. Don't.")
    main([str(notes)])
    assert capsys.readouterr().out == "don't   2\npanic   1\n"


def test_says_why_when_there_is_no_file(capsys):
    assert main(["nothing.txt"]) == 1
    assert capsys.readouterr().err == "wordfreq: nothing.txt: no such file\n"
# ANCHOR_END: fakeWorld


# ANCHOR: dedupe
# Python (a property with Hypothesis)
# Python（Hypothesis で性質）
def dedupe(xs):
    out = []
    for x in xs:
        if not out or out[-1] != x:
            out.append(x)
    return out


@given(st.lists(st.integers()))
def test_sort_keeps_the_length(xs):
    assert len(sorted(xs)) == len(xs)


def test_dedupe_finds_the_smallest_counterexample():
    # find: the smallest value that makes the condition true.
    # find：条件を真にする、いちばん小さい値
    smallest = find(st.lists(st.integers()),
                    lambda xs: len(set(dedupe(xs))) != len(dedupe(xs)),
                    settings=settings(derandomize=True, database=None))
    assert smallest == [0, 1, 0]
# ANCHOR_END: dedupe
