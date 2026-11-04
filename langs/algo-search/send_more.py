# SEND + MORE = MONEY, for the chapter "Search".
# SEND + MORE = MONEY。「探索」の章
from itertools import permutations


def number(digits):
    total = 0
    for d in digits:
        total = total * 10 + d
    return total


# ANCHOR: procedural
# Python (procedural style: loop over every assignment, collect answers)
# Python（手続き型スタイル：すべての割り当てをループで試し、答えを集める）
def solve():
    answers = []
    for s, e, n, d, m, o, r, y in permutations(range(10), 8):
        if s == 0 or m == 0:
            continue
        send = number([s, e, n, d])
        more = number([m, o, r, e])
        money = number([m, o, n, e, y])
        if send + more == money:
            answers.append((send, more, money))
    return answers
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: a lazy generator; next takes the first answer)
# Python（関数型スタイル：遅延なジェネレーター。next で最初の答えだけ）
def answers_lazily():
    return ((number([s, e, n, d]), number([m, o, r, e]),
             number([m, o, n, e, y]))
            for s, e, n, d, m, o, r, y in permutations(range(10), 8)
            if s != 0 and m != 0
            and number([s, e, n, d]) + number([m, o, r, e])
            == number([m, o, n, e, y]))


first = next(answers_lazily())
# ANCHOR_END: functional

assert solve() == [(9567, 1085, 10652)]
assert first == (9567, 1085, 10652)
print("ok")
