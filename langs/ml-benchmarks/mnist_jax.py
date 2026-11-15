# The real MNIST in JAX, with the same first weights, the same order and
# the same mini-batches as train-mnist (MiniTorch) and mnist.py (PyTorch).
# Skipped when the data has not been downloaded.
# 本物の MNIST を JAX で。最初の重み・順番・ミニバッチは、train-mnist
# （MiniTorch）と mnist.py（PyTorch）と同じ。データがなければ飛ばす
import contextlib
import io
import sys
import time
from pathlib import Path

import numpy as np

DATA = Path(__file__).parent.parent.parent / "ml" / "data" / "mnist"
if not (DATA / "train-images-idx3-ubyte").exists():
    print("skip: MNIST is not downloaded")
    sys.exit(0)

sys.path.insert(0, str(Path(__file__).parent.parent / "ml-neural-network"))
with contextlib.redirect_stdout(io.StringIO()):
    from digits import init_weights   # the same random numbers / 同じ乱数

import jax
import jax.numpy as jnp

jax.config.update("jax_enable_x64", True)


def load(prefix):
    images = np.fromfile(DATA / f"{prefix}-images-idx3-ubyte",
                         dtype=np.uint8, offset=16)
    labels = np.fromfile(DATA / f"{prefix}-labels-idx1-ubyte",
                         dtype=np.uint8, offset=8)
    return images.reshape(-1, 784) / 255.0, labels.astype(np.int64)


# ANCHOR: model
# Python (JAX): the model and the loss are plain functions of the
# parameters. jax.grad turns the loss into the function of its gradient.
# Python（JAX）：モデルと損失は、パラメーターを受け取るただの関数。
# jax.grad が、損失の関数を、その傾きの関数に変える
def mlp(params, x):
    h = jax.nn.relu(x @ params["fc1.w"] + params["fc1.b"])
    return h @ params["fc2.w"] + params["fc2.b"]


def loss_fn(params, x, y):
    logits = mlp(params, x)
    log_p = jax.nn.log_softmax(logits)
    return -jnp.mean(log_p[jnp.arange(y.shape[0]), y])


@jax.jit
def step(params, x, y):
    loss, grads = jax.value_and_grad(loss_fn)(params, x, y)
    new = {k: params[k] - 0.1 * grads[k] for k in params}
    return new, loss
# ANCHOR_END: model


x_train, y_train = map(jnp.array, load("train"))
x_test, y_test = map(jnp.array, load("t10k"))
params = {
    "fc1.w": jnp.array(init_weights(7, 784, 64)),
    "fc1.b": jnp.zeros(64),
    "fc2.w": jnp.array(init_weights(8, 64, 10)),
    "fc2.b": jnp.zeros(10),
}

# The first call compiles step (jit). Time it apart from the epoch.
# 最初の呼び出しで step をコンパイルする（jit）。1周の時間とは分けて測る
t0 = time.perf_counter()
jax.block_until_ready(step(params, x_train[:100], y_train[:100]))
compile_s = time.perf_counter() - t0

start = time.perf_counter()
losses = []
for k in range(600):
    params, loss = step(params, x_train[k * 100:(k + 1) * 100],
                        y_train[k * 100:(k + 1) * 100])
    if k % 100 == 0:
        print(f"batch {k:3d}  loss {float(loss):.4f}")
        losses.append(float(loss))
jax.block_until_ready(params)
elapsed = time.perf_counter() - start

acc = float(jnp.mean(jnp.argmax(mlp(params, x_test), axis=1) == y_test))
print(f"test accuracy {acc:.4f} (one epoch in {elapsed:.2f} s, compile {compile_s:.2f} s)")
assert abs(losses[0] - 2.2870) < 5e-5 and abs(losses[5] - 0.3831) < 5e-5
assert abs(acc - 0.9102) < 5e-5
