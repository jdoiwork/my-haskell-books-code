# The Python code in the measuring and tuning chapter.
# 計測とチューニングの章の Python のコード
import cProfile
import io
import pstats
import timeit

hits = [(f"hero-{i % 2000}", (i * 37) % 100) for i in range(1, 20001)]


# ANCHOR: timeit
# Python (timeit: run it many times, and take the time)
# Python（timeit：何回も動かして、時間を測る）
def heroes_slow(hits):
    seen = []
    for name, _ in hits:
        if name not in seen:
            seen.append(name)
    return seen


def heroes_fast(hits):
    return list(dict.fromkeys(name for name, _ in hits))


slow = timeit.timeit(lambda: heroes_slow(hits), number=3) / 3
fast = timeit.timeit(lambda: heroes_fast(hits), number=3) / 3
# ANCHOR_END: timeit
assert heroes_slow(hits) == heroes_fast(hits)
assert len(heroes_fast(hits)) == 2000
assert fast < slow

# ANCHOR: cprofile
# Python (cProfile: which functions took the time?)
# Python（cProfile：どの関数に時間がかかった？）
profiler = cProfile.Profile()
profiler.enable()
heroes_slow(hits)
profiler.disable()
pstats.Stats(profiler, stream=io.StringIO()).sort_stats("cumtime")
# ANCHOR_END: cprofile
print("ok")
