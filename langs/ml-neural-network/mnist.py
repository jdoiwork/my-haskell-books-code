# The real MNIST in PyTorch, with the same first weights, the same order
# and the same mini-batches as train-mnist (MiniTorch).
# Skipped when the data has not been downloaded.
# 本物の MNIST を PyTorch で。
# 最初の重み・順番・ミニバッチは train-mnist と同じ。
# データをダウンロードしていなければ飛ばす
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

sys.path.insert(0, str(Path(__file__).parent))
with contextlib.redirect_stdout(io.StringIO()):
    from digits import init_weights   # the same random numbers / 同じ乱数

import torch
from torch import nn


# ANCHOR: load
# Python (NumPy reads the IDX files)
# Python（NumPy で IDX ファイルを読む）
def load(prefix):
    images = np.fromfile(DATA / f"{prefix}-images-idx3-ubyte",
                         dtype=np.uint8, offset=16)
    labels = np.fromfile(DATA / f"{prefix}-labels-idx1-ubyte",
                         dtype=np.uint8, offset=8)
    return images.reshape(-1, 784) / 255.0, labels.astype(np.int64)
# ANCHOR_END: load


torch.set_default_dtype(torch.float64)
x_train, y_train = map(torch.tensor, load("train"))
x_test, y_test = map(torch.tensor, load("t10k"))

model = nn.Sequential(nn.Linear(784, 64), nn.ReLU(), nn.Linear(64, 10))
with torch.no_grad():
    model[0].weight.copy_(torch.tensor(init_weights(7, 784, 64)).T)
    model[0].bias.zero_()
    model[2].weight.copy_(torch.tensor(init_weights(8, 64, 10)).T)
    model[2].bias.zero_()
optimizer = torch.optim.SGD(model.parameters(), lr=0.1)

start = time.time()
losses = []
for k in range(600):
    xb = x_train[k * 100:(k + 1) * 100]
    yb = y_train[k * 100:(k + 1) * 100]
    loss = nn.functional.cross_entropy(model(xb), yb)
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()
    if k % 100 == 0:
        print(f"batch {k:3d}  loss {loss.item():.4f}")
        losses.append(loss.item())
elapsed = time.time() - start

with torch.no_grad():
    acc = (model(x_test).argmax(dim=1) == y_test).double().mean().item()
print(f"test accuracy {acc:.4f} (one epoch in {elapsed:.1f} s)")
assert abs(losses[0] - 2.2870) < 5e-5 and abs(losses[5] - 0.3831) < 5e-5
assert abs(acc - 0.9102) < 5e-5
