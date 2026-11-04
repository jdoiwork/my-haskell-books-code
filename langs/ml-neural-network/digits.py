# The Python code in the neural network chapter.
# ニューラルネットワークの章の Python のコード
import math
import time

GLYPHS = [
    ["01110", "10001", "10011", "10101", "11001", "10001", "01110"],
    ["00100", "01100", "00100", "00100", "00100", "00100", "01110"],
    ["01110", "10001", "00001", "00010", "00100", "01000", "11111"],
    ["11111", "00010", "00100", "00010", "00001", "10001", "01110"],
    ["00010", "00110", "01010", "10010", "11111", "00010", "00010"],
    ["11111", "10000", "11110", "00001", "00001", "10001", "01110"],
    ["00110", "01000", "10000", "11110", "10001", "10001", "01110"],
    ["11111", "00001", "00010", "00100", "01000", "01000", "01000"],
    ["01110", "10001", "10001", "01110", "10001", "10001", "01110"],
    ["01110", "10001", "10001", "01111", "00001", "00010", "01100"],
]
MASK = (1 << 64) - 1

def uniforms(seed):
    # The same generator as MiniTorch.Random.
    # MiniTorch.Random と同じ乱数
    x = seed
    while True:
        x = (x * 6364136223846793005 + 1442695040888963407) & MASK
        yield (x >> 11) / 9007199254740992

def make_digits(seed, noise, n):
    us = uniforms(seed)
    xs, labels = [], []
    for i in range(n):
        d = i % 10
        clean = [1.0 if c == "1" else 0.0 for row in GLYPHS[d] for c in row]
        xs.append([1.0 - p if next(us) < noise else p for p in clean])
        labels.append(d)
    return xs, labels

def init_weights(seed, inputs, outputs):
    a = 1 / math.sqrt(inputs)
    us = uniforms(seed)
    return [[(2 * next(us) - 1) * a for _ in range(outputs)]
            for _ in range(inputs)]

x_train, y_train = make_digits(1, 0.1, 1000)
x_test, y_test = make_digits(2, 0.1, 200)

# ANCHOR: torch
# Python (PyTorch: the same network, the same training loop)
# Python（PyTorch：同じネットワークと、同じ学習のループ）
import torch
from torch import nn

torch.set_default_dtype(torch.float64)
model = nn.Sequential(nn.Linear(35, 32), nn.ReLU(), nn.Linear(32, 10))
loss_fn = nn.CrossEntropyLoss()
optimizer = torch.optim.SGD(model.parameters(), lr=0.5)
# ANCHOR_END: torch

# Use the same starting weights as the Haskell version.
# Haskell 版と同じ、最初の重みを使う
with torch.no_grad():
    model[0].weight.copy_(torch.tensor(init_weights(7, 35, 32)).T)
    model[0].bias.zero_()
    model[2].weight.copy_(torch.tensor(init_weights(8, 32, 10)).T)
    model[2].bias.zero_()

X = torch.tensor(x_train)
Y = torch.tensor(y_train)
start = time.time()
# ANCHOR: loop
for epoch in range(301):
    loss = loss_fn(model(X), Y)      # forward and loss / 前向きと損失
    optimizer.zero_grad()
    loss.backward()                  # backward / 傾きを求める
    optimizer.step()                 # one step of SGD / SGD の1歩
    if epoch % 50 == 0:
        print(f"epoch {epoch:3d}  loss {loss.item():.4f}")
# ANCHOR_END: loop
elapsed = time.time() - start

with torch.no_grad():
    guesses = model(torch.tensor(x_test)).argmax(dim=1)
    acc = (guesses == torch.tensor(y_test)).double().mean().item()
print(f"test accuracy {acc:.3f} ({elapsed:.2f} s)")
assert abs(loss.item() - 0.0131) < 0.0005
assert abs(acc - 0.985) < 1e-9
