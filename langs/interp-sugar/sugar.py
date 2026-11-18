# The Python code in the chapter on patterns and sugar.
# パターンと糖衣構文の章の Python のコード
scores = [("Python", 82), ("TS", 75), ("Rust", 91), ("Go", 68)]


# ANCHOR: procedural
# Python (procedural style): a for loop, taking each pair apart
# Python（手続き型スタイル）：for 文で、組を1つずつほどく
def report_procedural(scores: list[tuple[str, int]]) -> tuple[list[str], int]:
    passed = []
    total = 0
    for name, score in scores:
        if score >= 70:
            passed.append(name)
        total += score
    return passed, total
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style): comprehensions with unpacking
# Python（関数型スタイル）：アンパックつきの内包表記
def report(scores: list[tuple[str, int]]) -> tuple[list[str], int]:
    passed = [name for name, score in scores if score >= 70]
    total = sum(score for _, score in scores)
    return passed, total


pairs = [(a, b) for a, _ in scores for b, _ in scores if a < b]

# A lambda cannot take a tuple apart in its arguments.
# ラムダは、引数で組をほどけない
bonus = map(lambda pair: (pair[0], min(pair[1] + 5, 100)), scores)
# ANCHOR_END: functional

assert report_procedural(scores) == (["Python", "TS", "Rust"], 316)
assert report(scores) == (["Python", "TS", "Rust"], 316)
assert len(pairs) == 6
assert list(bonus) == [("Python", 87), ("TS", 80), ("Rust", 96), ("Go", 73)]

# In Python 3, a tuple in the parameters is a syntax error (PEP 3113).
# Python 3 では、引数に組を書くと構文エラー（PEP 3113）
try:
    compile("lambda (name, score): name", "<test>", "eval")
    raise AssertionError("should not compile")
except SyntaxError:
    pass
