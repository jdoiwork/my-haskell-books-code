# Slopes and gradient descent of the chapter "Derivatives and gradients",
# in Python.
# 「微分と勾配」の章の傾きと勾配降下法を、Python で
import torch


def height(x, y):
    return (x - 3) ** 2 + 2 * (y + 1) ** 2


def derivative(f, x, h=1e-5):
    return (f(x + h) - f(x - h)) / (2 * h)


# ANCHOR: procedural
# Python (procedural style: walk down the valley)
# Python（手続き型スタイル：くぼ地を下る）
x, y = 0.0, 0.0
rate = 0.1
for step in range(100):
    gx = derivative(lambda t: height(t, y), x)
    gy = derivative(lambda t: height(x, t), y)
    x, y = x - rate * gx, y - rate * gy
# ANCHOR_END: procedural
assert abs(x - 3) < 1e-6 and abs(y + 1) < 1e-6

# ANCHOR: functional
# Python (PyTorch: the slope by automatic differentiation)
# Python（PyTorch：自動微分で傾きを求める）
p = torch.tensor(10.0, requires_grad=True)
revenue = p * (100 - 2 * p)
revenue.backward()
slope_at_10 = p.grad.item()     # 60.0: exact, no h needed
# ANCHOR_END: functional
assert slope_at_10 == 60.0
assert abs(derivative(lambda q: q * (100 - 2 * q), 10.0) - 60) < 1e-6
print("ok")
