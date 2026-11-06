# The Python code in the "rewrite to go faster" chapter.
# 書き換えて速くするの章の Python のコード
import numpy as np

# ANCHOR: procedural
# Python (procedural style: a list of bools, rewritten in place)
# Python（手続き型スタイル：bool のリストを、その場で書き換える）
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
    return is_prime
# ANCHOR_END: procedural

# ANCHOR: numpy
# Python (NumPy: cross out all the multiples with one slice)
# Python（NumPy：倍数を、スライス1つでまとめて消す）
def sieve_np(n):
    is_prime = np.ones(n + 1, dtype=bool)
    is_prime[:2] = False
    p = 2
    while p * p <= n:
        if is_prime[p]:
            is_prime[p * p::p] = False
        p += 1
    return is_prime
# ANCHOR_END: numpy

primes = [i for i, ok in enumerate(sieve(30)) if ok]
assert primes == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
assert list(np.flatnonzero(sieve_np(30))) == primes
assert sum(sieve(1000000)) == 78498
assert int(sieve_np(1000000).sum()) == 78498
print("ok")
