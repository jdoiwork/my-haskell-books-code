# The Python code in the data structures chapter.
# データ構造の選び方の章の Python のコード

# ANCHOR: procedural
# Python (a list: pop(0) moves every other item)
# Python（リスト：pop(0) は、残りを全部ずらす）
def take_turns_list(n, heroes):
    line = list(heroes)
    order = []
    for _ in range(n):
        if not line:
            break
        hero = line.pop(0)
        order.append(hero)
        line.append(hero)
    return order
# ANCHOR_END: procedural

# ANCHOR: deque
# Python (deque: fast at both ends)
# Python（deque：両端が速い）
from collections import deque

def take_turns(n, heroes):
    line = deque(heroes)
    order = []
    for _ in range(n):
        if not line:
            break
        hero = line.popleft()
        order.append(hero)
        line.append(hero)
    return order
# ANCHOR_END: deque

heroes = ["Lambda", "Python", "Rust"]
expected = ["Lambda", "Python", "Rust", "Lambda", "Python", "Rust", "Lambda"]
assert take_turns_list(7, heroes) == expected
assert take_turns(7, heroes) == expected
assert take_turns_list(3, []) == []
assert take_turns(3, []) == []
print("ok")
