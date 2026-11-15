# Softmax of each row of a 1000 × 1000 matrix in Python: loops over
# lists, NumPy, PyTorch and JAX.
# 1000 × 1000 の行列の、行ごとの softmax を Python で：リストのループ、
# NumPy、PyTorch、JAX
import math

import numpy as np
import torch
import jax
import jax.numpy as jnp

from timer import FULL, median_ms, report

jax.config.update("jax_enable_x64", True)


# ANCHOR: loops
# Python (procedural style)
# Python（手続き型スタイル）
def softmax_rows_loops(x):
    out = []
    for row in x:
        m = max(row)
        e = [math.exp(t - m) for t in row]
        s = sum(e)
        out.append([v / s for v in e])
    return out
# ANCHOR_END: loops


# ANCHOR: numpy
# Python (NumPy): each step makes a new array
# Python（NumPy）：1歩ごとに、新しい配列ができる
def softmax_rows_numpy(x):
    e = np.exp(x - x.max(axis=1, keepdims=True))
    return e / e.sum(axis=1, keepdims=True)
# ANCHOR_END: numpy


@jax.jit
def softmax_rows_jax(x):
    e = jnp.exp(x - x.max(axis=1, keepdims=True))
    return e / e.sum(axis=1, keepdims=True)


# Check the four ways against each other on a small matrix.
# 小さな行列で、4つの書き方の答えをくらべる
small = np.random.default_rng(3).random((5, 7))
want = softmax_rows_numpy(small)
assert np.allclose(want.sum(axis=1), 1.0)
assert np.allclose(softmax_rows_loops(small.tolist()), want)
assert np.allclose(torch.softmax(torch.tensor(small), dim=1).numpy(), want)
assert np.allclose(np.asarray(softmax_rows_jax(jnp.array(small))), want)

n = 1000 if FULL else 50
x = np.random.default_rng(3).random((n, n))
report("softmax", n, "Python loops",
       median_ms(3, lambda v: sum(map(sum, softmax_rows_loops(v))), x.tolist()))
report("softmax", n, "NumPy",
       median_ms(10, lambda v: softmax_rows_numpy(v).sum(), x))
report("softmax", n, "PyTorch",
       median_ms(10, lambda v: torch.softmax(v, dim=1).sum().item(), torch.tensor(x)))
report("softmax", n, "JAX",
       median_ms(10, lambda v: softmax_rows_jax(v).sum().block_until_ready(), jnp.array(x)))
