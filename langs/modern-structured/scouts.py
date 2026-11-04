# Scouts sent out at the same time, for the chapter
# "Structured concurrency".
# 同時に送り出す偵察隊。「構造化された並行性」の章
import asyncio

report = []


async def scout(name, seconds):
    await asyncio.sleep(seconds)
    report.append(name)
    return name


async def trap(seconds):
    await asyncio.sleep(seconds)
    raise RuntimeError("fell into a trap")


# ANCHOR: procedural
# Python (create_task: the tasks run on, nobody waits for them)
# Python（create_task：タスクは走り続け、だれも待たない）
async def loose_party():
    tasks = [
        asyncio.create_task(scout("slime", 0.05)),
        asyncio.create_task(trap(0.01)),
        asyncio.create_task(scout("goblin", 0.1)),
    ]
    return tasks
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (TaskGroup: wait for all; one failure cancels the rest)
# Python（TaskGroup：全員を待つ。1つ失敗すると、残りはキャンセル）
async def structured_party():
    async with asyncio.TaskGroup() as group:
        group.create_task(scout("slime", 0.05))
        group.create_task(trap(0.01))
        group.create_task(scout("goblin", 0.1))
# ANCHOR_END: functional


async def main():
    tasks = await loose_party()
    assert report == []
    await asyncio.sleep(0.2)
    assert report == ["slime", "goblin"]
    assert isinstance(tasks[1].exception(), RuntimeError)

    report.clear()
    try:
        await structured_party()
        assert False, "should have failed"
    except* RuntimeError:
        pass
    await asyncio.sleep(0.2)
    assert report == []
    print("ok")


asyncio.run(main())
