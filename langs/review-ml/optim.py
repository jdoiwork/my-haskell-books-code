# The Python code of the Part VIII review: the digit network trained for
# 100 steps with torch.optim's SGD, SGD with momentum, and Adam.
# The data and the first weights are the same as the Haskell version.
# 第VIII部のふりかえりの Python のコード：数字のネットワークを、
# torch.optim の SGD、Momentum 付きの SGD、Adam で 100 歩ずつ学ばせる。
# データと最初の重みは、Haskell 版と同じ
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent / "ml-neural-network"))
import contextlib, io  # noqa: E401
with contextlib.redirect_stdout(io.StringIO()):
    # Reuse the data and weights of the neural network chapter
    # (and hide what that file prints).
    # ニューラルネットワークの章のデータと重みを使い回す（表示は隠す）
    from digits import init_weights, make_digits

import torch
from torch import nn

torch.set_default_dtype(torch.float64)
x_train, y_train = make_digits(1, 0.1, 1000)
x_test, y_test = make_digits(2, 0.1, 200)
X, Y = torch.tensor(x_train), torch.tensor(y_train)


def fresh_model():
    model = nn.Sequential(nn.Linear(35, 32), nn.ReLU(), nn.Linear(32, 10))
    with torch.no_grad():
        model[0].weight.copy_(torch.tensor(init_weights(7, 35, 32)).T)
        model[0].bias.zero_()
        model[2].weight.copy_(torch.tensor(init_weights(8, 32, 10)).T)
        model[2].bias.zero_()
    return model


results = {}
for name in ["SGD", "Momentum", "Adam"]:
    model = fresh_model()
    # ANCHOR: optimizers
    # Python (PyTorch)
    if name == "SGD":
        optimizer = torch.optim.SGD(model.parameters(), lr=0.5)
    elif name == "Momentum":
        optimizer = torch.optim.SGD(model.parameters(), lr=0.1, momentum=0.9)
    else:
        optimizer = torch.optim.Adam(model.parameters(), lr=0.01)
    # ANCHOR_END: optimizers
    losses = []
    for _ in range(101):
        loss = nn.functional.cross_entropy(model(X), Y)
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        losses.append(loss.item())
    with torch.no_grad():
        guesses = model(torch.tensor(x_test)).argmax(dim=1)
        acc = (guesses == torch.tensor(y_test)).double().mean().item()
    shown = " ".join(f"{losses[k]:.4f}" for k in [0, 25, 50, 75, 100])
    print(f"{name:8}  loss {shown}  test {acc:.3f}")
    results[name] = (losses, acc)

assert math.isclose(results["SGD"][0][100], 0.0414, abs_tol=5e-5)
assert math.isclose(results["Momentum"][0][100], 0.0149, abs_tol=5e-5)
assert math.isclose(results["Adam"][0][100], 0.0080, abs_tol=5e-5)
assert all(acc == 0.99 for _, acc in results.values())
