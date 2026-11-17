# Python: a pool of workers with asyncio.Queue (the same as pool.lam).
# First with a "no more" mark (None) for each worker, then with
# Queue.shutdown (Python 3.13), which is like closing a channel.
# Python：asyncio.Queue で働き手の組（pool.lam と同じ）。はじめは働き手ごと
# の「もうない」の印（None）で、次に Queue.shutdown（Python 3.13）で。
# shutdown は、チャネルを閉じるのに近い
import asyncio

names = ["bulbasaur", "charmander", "squirtle", "pikachu"]


async def fetch(name):
    await asyncio.sleep(0.01)
    return name.upper()


# ANCHOR: procedural
# Python (procedural style): put a None for each worker at the end
# Python（手続き型スタイル）：最後に、働き手ごとに None を入れる
async def pool_with_marks():
    todo = asyncio.Queue()
    results = []

    async def worker(id):
        while True:
            name = await todo.get()
            if name is None:
                return
            results.append(f"worker {id}: {await fetch(name)}")

    for name in names:
        await todo.put(name)
    for _ in range(3):
        await todo.put(None)
    await asyncio.gather(*(worker(id) for id in [1, 2, 3]))
    return results
# ANCHOR_END: procedural


# ANCHOR: shutdown
# Python (3.13 and later): shutdown() closes the queue; get() then
# raises QueueShutDown once it is empty, like Chan.recv giving Nothing
# Python（3.13 から）：shutdown() でキューを閉じる。空になると get() が
# QueueShutDown を投げる。Chan.recv が Nothing を返すのと同じ
async def pool_with_shutdown():
    todo = asyncio.Queue()

    async def worker(id):
        done = []
        try:
            while True:
                done.append(f"worker {id}: {await fetch(await todo.get())}")
        except asyncio.QueueShutDown:
            return done

    for name in names:
        todo.put_nowait(name)
    todo.shutdown()
    piles = await asyncio.gather(*(worker(id) for id in [1, 2, 3]))
    return [line for pile in piles for line in pile]
# ANCHOR_END: shutdown


def names_in(lines):
    return sorted(line.split(": ")[1] for line in lines)


assert names_in(asyncio.run(pool_with_marks())) == sorted(n.upper() for n in names)
assert names_in(asyncio.run(pool_with_shutdown())) == sorted(n.upper() for n in names)

# Sending after shutdown is a mistake, as in Go (and Lam).
# shutdown のあとに入れるのはまちがい。Go（と Lam）と同じ


async def put_after_shutdown():
    q = asyncio.Queue()
    q.shutdown()
    try:
        q.put_nowait(1)
    except asyncio.QueueShutDown:
        return "refused"


assert asyncio.run(put_after_shutdown()) == "refused"
print("ok")
