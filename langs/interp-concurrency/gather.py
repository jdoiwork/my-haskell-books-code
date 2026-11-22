# Python (asyncio): many waits at once, the ways to take a failure, and
# a time limit. "fetch" only sleeps, so this runs without a network.
# Python（asyncio）：たくさんの待ちを同時に、失敗の受け止め方、制限時間。
# fetch は眠るだけなので、ネットなしで動く
import asyncio
import time

DELAYS = {"bulbasaur": 0.3, "charmander": 0.5, "squirtle": 0.2,
          "pikachu": 0.4, "eevee": 0.3, "snorlax": 0.6}
finished: list[str] = []


async def fetch(name: str) -> str:
    await asyncio.sleep(DELAYS.get(name, 0.1))
    if name not in DELAYS:
        raise LookupError(f"{name}: 404")
    finished.append(name)
    return name.upper()


# ANCHOR: procedural
# Python (procedural style): await one by one
# Python（手続き型スタイル）：1つずつ await する
async def one_by_one(names: list[str]) -> list[str]:
    result = []
    for name in names:
        result.append(await fetch(name))
    return result
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style): gather starts them all, then waits
# Python（関数型スタイル）：gather が全部を始めてから、待つ
async def all_at_once(names: list[str]) -> list[str]:
    return await asyncio.gather(*(fetch(n) for n in names))
# ANCHOR_END: functional


# ANCHOR: limit
# At most n at once: a semaphore (IO.allN n)
# 同時に多くて n 個：セマフォで（IO.allN n）
async def at_most(n: int, names: list[str]) -> list[str]:
    tickets = asyncio.Semaphore(n)

    async def one(name: str) -> str:
        async with tickets:
            return await fetch(name)

    return await asyncio.gather(*(one(name) for name in names))
# ANCHOR_END: limit


# ANCHOR: failures
# IO.settle: return_exceptions=True gives the errors as answers
# IO.settle：return_exceptions=True なら、エラーも答えとして返る
async def settle(names: list[str]) -> tuple[list[str], list[str]]:
    results = await asyncio.gather(*(fetch(n) for n in names),
                                   return_exceptions=True)
    oks = [r for r in results if isinstance(r, str)]
    errs = [str(r) for r in results if isinstance(r, Exception)]
    return oks, errs


# IO.allOk: TaskGroup cancels the others at the first error
# IO.allOk：TaskGroup は、最初のエラーでほかを取り消す
async def all_ok(names: list[str]) -> list[str]:
    async with asyncio.TaskGroup() as group:
        tasks = [group.create_task(fetch(n)) for n in names]
    return [t.result() for t in tasks]
# ANCHOR_END: failures


# ANCHOR: timeout
# IO.timeout: wait at most 0.5 s
# IO.timeout：多くて 0.5 秒だけ待つ
async def within(name: str) -> str | None:
    try:
        async with asyncio.timeout(0.5):
            return await fetch(name)
    except TimeoutError:
        return None
# ANCHOR_END: timeout


# ANCHOR: cleanup
# A cancelled task gets CancelledError where it waits, so its finally
# still runs (as defer of Lam does)
# 取り消されたタスクは、待っている所で CancelledError を受け取るので、
# finally は動く（Lam の defer と同じ）
async def save(parts: list[str]) -> None:
    parts.append("report.txt.part")
    try:
        await asyncio.sleep(2.0)            # the slow rest / 遅い残り
        parts.append("report.txt")
    finally:
        parts.remove("report.txt.part")
# ANCHOR_END: cleanup


async def timed(coro) -> tuple[object, float]:
    start = time.perf_counter()
    value = await coro
    return value, time.perf_counter() - start


async def main() -> None:
    names = list(DELAYS)
    slow, t1 = await timed(one_by_one(names))
    fast, t2 = await timed(all_at_once(names))
    two, t3 = await timed(at_most(2, names))
    assert slow == fast == two == [n.upper() for n in names]
    assert 2.25 < t1 < 2.6, t1      # 0.3 + 0.5 + ... = 2.3
    assert 0.55 < t2 < 0.8, t2      # the longest: 0.6
    assert 1.35 < t3 < 1.6, t3      # two at a time: 1.4

    oks, errs = await settle(["pikachu", "nosuchmon", "eevee"])
    assert oks == ["PIKACHU", "EEVEE"] and errs == ["nosuchmon: 404"]

    # TaskGroup: the error at 0.1 s cancels snorlax (0.6 s).
    # TaskGroup：0.1 秒のエラーが snorlax（0.6 秒）を取り消す
    finished.clear()
    try:
        await all_ok(["snorlax", "nosuchmon"])
        assert False, "no error"
    except* LookupError:
        pass
    await asyncio.sleep(0.7)
    assert "snorlax" not in finished

    # gather (without return_exceptions) does NOT cancel the others:
    # snorlax still finishes after the error.
    # gather（return_exceptions なし）は、ほかを取り消さない。エラーの
    # あとも snorlax は最後まで動く
    finished.clear()
    try:
        await asyncio.gather(fetch("snorlax"), fetch("nosuchmon"))
        assert False, "no error"
    except LookupError:
        pass
    await asyncio.sleep(0.7)
    assert "snorlax" in finished

    assert await within("pikachu") == "PIKACHU"
    assert await within("snorlax") is None

    # The time limit cancels the save, and its finally cleans up.
    # 制限時間が保存を取り消し、finally が片付ける
    parts: list[str] = []
    try:
        async with asyncio.timeout(0.3):
            await save(parts)
    except TimeoutError:
        pass
    assert parts == [], parts
    print("ok")


asyncio.run(main())
