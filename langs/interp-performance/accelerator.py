# Python: heapq is written in Python, and the C version takes its names
# Python：heapq は Python で書いてあり、C の版がその名前を取る
import heapq
import inspect

# ANCHOR: accelerator
# Python (the end of heapq.py of the standard library)
# Python（標準ライブラリの heapq.py の最後）
try:
    from _heapq import *
except ImportError:
    pass
# ANCHOR_END: accelerator

# ANCHOR: which
# Python
# The name heappush is the one of C (_heapq), not of heapq.py.
# heappush という名前は、heapq.py のものではなく、C のもの（_heapq）
print(heapq.heappush.__module__)  # _heapq
# ANCHOR_END: which

assert heapq.heappush.__module__ == "_heapq"
source = inspect.getsource(heapq)
assert "def heappush(heap, item):" in source
assert "from _heapq import *" in source
# The two versions give the same answer.
# 2つの版は、同じ答えを返す
xs = [(i * 7919) % 100003 for i in range(1, 2001)]
fast = []
for x in xs:
    heapq.heappush(fast, x)
assert [heapq.heappop(fast) for _ in range(len(xs))] == sorted(xs)
