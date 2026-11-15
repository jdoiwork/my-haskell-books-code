# The matrix product of two n × n matrices in Python: three loops,
# NumPy, PyTorch and JAX. Every way returns the sum of the answer, like
# checksum in Haskell, so that nobody can skip work.
# n × n の行列の積を Python で：3重ループ、NumPy、PyTorch、JAX。
# どれも答えの合計を返す（Haskell の checksum と同じ。仕事を省けないように）
import random

import numpy as np
import torch
import jax
import jax.numpy as jnp

from timer import FULL, median_ms, report

jax.config.update("jax_enable_x64", True)  # Double, like the others / ほかと同じ Double


# ANCHOR: naive
# Python (procedural style): three loops over lists
# Python（手続き型スタイル）：リストの上の3重ループ
def naive_matmul(a, b):
    n, k, m = len(a), len(b), len(b[0])
    c = [[0.0] * m for _ in range(n)]
    for i in range(n):
        for j in range(m):
            s = 0.0
            for t in range(k):
                s += a[i][t] * b[t][j]
            c[i][j] = s
    return c
# ANCHOR_END: naive


# ANCHOR: libs
# Python (NumPy, PyTorch, JAX): one operator, and BLAS does the work
# Python（NumPy・PyTorch・JAX）：演算子1つで、あとは BLAS が計算する
def numpy_matmul(a, b):
    return (a @ b).sum()


def torch_matmul(a, b):
    return (a @ b).sum().item()


@jax.jit
def jax_sum_of_product(a, b):
    return (a @ b).sum()


def jax_matmul(a, b):
    # JAX returns at once and computes in the background:
    # wait for the answer, or the clock stops too early.
    # JAX はすぐに戻って、裏で計算する。答えを待たないと、時計が早く止まる
    return jax_sum_of_product(a, b).block_until_ready()
# ANCHOR_END: libs


# Check: every way gives the same answer for a small matrix.
# 確かめ：小さな行列なら、どの書き方も同じ答え
rng = random.Random(1)
small_a = [[rng.random() for _ in range(20)] for _ in range(20)]
small_b = [[rng.random() for _ in range(20)] for _ in range(20)]
want = sum(map(sum, naive_matmul(small_a, small_b)))
na, nb = np.array(small_a), np.array(small_b)
assert abs(numpy_matmul(na, nb) - want) < 1e-9
assert abs(torch_matmul(torch.tensor(na), torch.tensor(nb)) - want) < 1e-9
assert abs(float(jax_matmul(jnp.array(na), jnp.array(nb))) - want) < 1e-9

for n in ((256, 512, 1024) if FULL else (32,)):
    a = np.random.default_rng(1).random((n, n))
    b = np.random.default_rng(2).random((n, n))
    if n <= 256:
        report("matmul", n, "Python naive",
               median_ms(3, naive_matmul, a.tolist(), b.tolist()))
    report("matmul", n, "NumPy", median_ms(10, numpy_matmul, a, b))
    report("matmul", n, "PyTorch",
           median_ms(10, torch_matmul, torch.tensor(a), torch.tensor(b)))
    report("matmul", n, "JAX",
           median_ms(10, jax_matmul, jnp.array(a), jnp.array(b)))
