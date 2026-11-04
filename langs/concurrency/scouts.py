# The Python code in the concurrency chapter.
# 並行・並列の章の Python のコード
import asyncio
import threading
import time


# ANCHOR: gather
# Python (asyncio: wait for many at once with gather)
# Python（asyncio：gather で、いくつもを同時に待つ）
async def scout(place, millis):
    await asyncio.sleep(millis / 1000)
    return f"{place}: {millis // 100} goblins"


async def scouts_at_once(places):
    return await asyncio.gather(*(scout(p, m) for p, m in places))
# ANCHOR_END: gather

places = [("forest", 300), ("cave", 200), ("tower", 100)]
start = time.monotonic()
reports = asyncio.run(scouts_at_once(places))
assert time.monotonic() - start < 0.5
assert reports == ["forest: 3 goblins", "cave: 2 goblins", "tower: 1 goblins"]


# ANCHOR: lock
# Python (threads and a Lock around the shared count)
# Python（スレッドと、共有する数を守る Lock）
count = 0
lock = threading.Lock()


def clerk():
    global count
    for _ in range(100000):
        with lock:
            count += 1


clerks = [threading.Thread(target=clerk) for _ in range(10)]
for c in clerks:
    c.start()
for c in clerks:
    c.join()
# ANCHOR_END: lock
assert count == 1000000
print("ok")
