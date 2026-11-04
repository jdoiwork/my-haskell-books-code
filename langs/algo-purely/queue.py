# Queues, for the chapter "Building purely functional data structures".
# キュー。「純粋なデータ構造を作る」の章
from collections import deque


# ANCHOR: procedural
# Python (procedural style: change the list in place)
# Python（手続き型スタイル：リストをその場で書き換える）
line = []
for name in ["slime", "goblin", "dragon"]:
    line.append(name)
first = line.pop(0)  # O(n): every name moves one step forward

turns = deque(["slime", "goblin", "dragon"])
turns.append("golem")
second = turns.popleft()  # O(1)
# ANCHOR_END: procedural

assert first == "slime" and line == ["goblin", "dragon"]
assert second == "slime" and list(turns) == ["goblin", "dragon", "golem"]


# ANCHOR: functional
# Python (functional style: two immutable lists made of pairs)
# Python（関数型スタイル：ペアでできた、変わらない2つのリスト）
EMPTY = (None, None)


def push(x, queue):
    front, back = queue
    return check((front, (x, back)))


def pop(queue):
    front, back = queue
    if front is None:
        return None
    x, rest = front
    return x, check((rest, back))


def check(queue):
    front, back = queue
    if front is not None:
        return queue
    turned = None
    while back is not None:
        x, back = back
        turned = (x, turned)
    return (turned, None)
# ANCHOR_END: functional


def drain(queue):
    out = []
    while (step := pop(queue)) is not None:
        x, queue = step
        out.append(x)
    return out


q0 = push("b", push("a", EMPTY))
q1 = push("c", q0)
q2 = push("z", q0)
assert drain(q1) == ["a", "b", "c"]
assert drain(q2) == ["a", "b", "z"]
assert drain(q0) == ["a", "b"]
print("ok")
