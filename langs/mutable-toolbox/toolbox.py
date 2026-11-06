# The Python code in the "choosing a box" chapter.
# 書き換えられる箱の使い分けの章の Python のコード
import threading
import time
from functools import cache
from queue import Queue

# ANCHOR: counter
# Python (procedural style: a closure, nonlocal, and a Lock)
# Python（手続き型スタイル：クロージャーと nonlocal と Lock）
def make_ticket_box():
    next_number = 1
    lock = threading.Lock()

    def take_ticket():
        nonlocal next_number
        with lock:
            n = next_number
            next_number += 1
        return n

    return take_ticket
# ANCHOR_END: counter

take_ticket = make_ticket_box()
got = []
def visitor():
    for _ in range(10):
        got.append(take_ticket())
threads = [threading.Thread(target=visitor) for _ in range(100)]
for t in threads:
    t.start()
for t in threads:
    t.join()
assert sorted(got) == list(range(1, 1001))

calls = 0

# ANCHOR: cache
# Python (functools.cache: remember the answers of a function)
# Python（functools.cache：関数の答えを覚えておく）
@cache
def appraise(item: str) -> int:
    global calls
    calls += 1
    return len(item) * 100
# ANCHOR_END: cache

assert [appraise("sword"), appraise("sword"), appraise("shield")] == \
    [500, 500, 600]
assert calls == 2

# ANCHOR: queue
# Python (queue.Queue: the board has room for 3; None means "closed")
# Python（queue.Queue：掲示板は3枚まで。None が「閉店」の合図）
def guild_day(clerks: int, quests: list[str]) -> list[tuple[int, str]]:
    board: Queue[str | None] = Queue(maxsize=3)
    done: list[tuple[int, str]] = []
    done_lock = threading.Lock()

    def clerk(me: int) -> None:
        while True:
            quest = board.get()
            if quest is None:
                return
            time.sleep(0.01)
            with done_lock:
                done.append((me, quest))

    workers = [threading.Thread(target=clerk, args=(i,))
               for i in range(1, clerks + 1)]
    for w in workers:
        w.start()
    for q in quests:
        board.put(q)
    for _ in workers:
        board.put(None)
    for w in workers:
        w.join()
    return done
# ANCHOR_END: queue

quests = [f"quest-{i}" for i in range(1, 10)]
results = guild_day(3, quests)
assert sorted(q for _, q in results) == sorted(quests)
print("ok")
