# A blocking pop in Python, for the chapter "Pub/Sub and blocking".
# 「Pub/Sub とブロッキング」の章の、待つ取り出しを Python で
import threading
import time

jobs = []
changed = threading.Condition()


# ANCHOR: procedural
# Python (procedural style: wait on a condition, wake up on notify)
# Python（手続き型スタイル：条件で待ち、notify で起きる）
def blpop(timeout):
    with changed:
        ready = changed.wait_for(lambda: len(jobs) > 0, timeout=timeout)
        return jobs.pop(0) if ready else None


def rpush(item):
    with changed:
        jobs.append(item)
        changed.notify_all()        # wake the waiters / 待っている人を起こす
# ANCHOR_END: procedural


result = []
waiter = threading.Thread(target=lambda: result.append(blpop(5)))
waiter.start()
time.sleep(0.1)
rpush("slay goblins")
waiter.join()
assert result == ["slay goblins"]
assert blpop(0.1) is None
print("ok")
