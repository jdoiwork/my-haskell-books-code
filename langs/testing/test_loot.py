# The pytest and Hypothesis code in the testing chapter.
# テストの章の pytest と Hypothesis のコード
def split_loot(gold, members):
    share, rest = divmod(gold, members)
    return [share + 1 if i < rest else share for i in range(members)]
# ANCHOR: example_tests_with_pytest
# Python (example tests with pytest)
# Python（pytest で例のテスト）
def test_split_100_among_4():
    assert split_loot(100, 4) == [25, 25, 25, 25]

def test_split_90_among_3():
    assert split_loot(90, 3) == [30, 30, 30]
# ANCHOR_END: example_tests_with_pytest
# ANCHOR: property_tests_with_hypothesis
# Python (property tests with Hypothesis)
# Python（Hypothesis で性質のテスト）
from hypothesis import given, strategies as st

@given(st.integers(min_value=0),
       st.integers(min_value=1, max_value=100))
def test_keeps_total(gold, members):
    assert sum(split_loot(gold, members)) == gold
# ANCHOR_END: property_tests_with_hypothesis
