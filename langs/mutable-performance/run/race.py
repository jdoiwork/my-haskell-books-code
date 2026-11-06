# The sieve race: how long does a sieve up to n take? (the best of 3)
# ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
#   python race.py list 10000000   /   python race.py numpy 10000000
import sys, time
import numpy as np


def sieve(n):
    is_prime = [True] * (n + 1)
    is_prime[0] = False
    is_prime[1] = False
    p = 2
    while p * p <= n:
        if is_prime[p]:
            for m in range(p * p, n + 1, p):
                is_prime[m] = False
        p += 1
    return sum(is_prime)


def sieve_np(n):
    is_prime = np.ones(n + 1, dtype=bool)
    is_prime[:2] = False
    p = 2
    while p * p <= n:
        if is_prime[p]:
            is_prime[p * p::p] = False
        p += 1
    return int(is_prime.sum())


run = {"list": sieve, "numpy": sieve_np}[sys.argv[1]]
n = int(sys.argv[2])
best = None
for _ in range(3):
    start = time.perf_counter()
    count = run(n)
    ms = (time.perf_counter() - start) * 1000
    best = ms if best is None else min(best, ms)
print(count, f"{best:.1f}")
