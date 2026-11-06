# The Python code in the "lifting IO" chapter.
# IO を持ち上げるの章の Python のコード
import asyncio
import time


def shout(message):
    print(f"[board] {message}")


# ANCHOR: inside
# Python (inside async: calling a plain function is easy)
# Python（async の中から、ふつうの関数を呼ぶのは簡単）
async def appraise(item):
    await asyncio.sleep(0.1)
    return len(item) * 100


async def appraise_all(items):
    shout("appraising...")  # a plain function / ふつうの関数
    return await asyncio.gather(*(appraise(i) for i in items))
# ANCHOR_END: inside


# ANCHOR: outside
# Python (from a plain function, async needs a runner: asyncio.run)
# Python（ふつうの関数から async を動かすには、ランナー asyncio.run が要る）
def closing_report(items):
    # prices = await appraise_all(items)  # SyntaxError outside async
    prices = asyncio.run(appraise_all(items))
    return sum(prices)
# ANCHOR_END: outside


start = time.perf_counter()
assert closing_report(["sword", "shield", "herb"]) == 1500
assert time.perf_counter() - start < 0.25, "at the same time"


# ANCHOR: nested
# Python (inside async, we cannot start a second runner)
# Python（async の中では、2つ目のランナーを動かせない）
async def evening():
    report = appraise_all(["herb"])
    try:
        return asyncio.run(report)
    except RuntimeError as error:
        report.close()  # it never ran / 一度も動かなかった
        return str(error)
# ANCHOR_END: nested


message = asyncio.run(evening())
assert "cannot be called from a running event loop" in message, message
print("ok")
