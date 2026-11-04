# Checking an answer with properties, for the chapter "Programming with AI".
# 性質で答えを確かめる。「AI と書くプログラミング」の章
from hypothesis import given
from hypothesis import strategies as st


# ANCHOR: procedural
# Python (procedural style: a plausible answer that drops the rest)
# Python（手続き型スタイル：余りを捨ててしまう、それらしい答え）
def chunks_draft(n, xs):
    groups = []
    i = 0
    while i + n <= len(xs):
        groups.append(xs[i:i + n])
        i += n
    return groups
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: a slice per step, plus a property test)
# Python（関数型スタイル：1歩ごとにスライス。性質のテストも）
def chunks(n, xs):
    return [xs[i:i + n] for i in range(0, len(xs), n)]


@given(st.integers(min_value=1, max_value=10), st.lists(st.integers()))
def test_chunks_keep_everything(n, xs):
    assert sum(chunks(n, xs), []) == xs
# ANCHOR_END: functional


def test_the_draft_loses_the_rest():
    assert chunks_draft(2, [1, 2, 3]) == [[1, 2]]
    assert chunks(2, [1, 2, 3]) == [[1, 2], [3]]
