# The Python code in the autograd chapter.
# 自動微分の章の Python のコード
import math

# ANCHOR: micrograd
# Python (a tiny autograd: each value keeps its grad and a _backward)
# Python（小さな autograd：値ごとに grad と _backward を持つ）
class Value:
    def __init__(self, data, parents=()):
        self.data = data
        self.grad = 0.0
        self._parents = parents
        self._backward = lambda: None

    def __add__(self, other):
        out = Value(self.data + other.data, (self, other))
        def _backward():
            self.grad += out.grad
            other.grad += out.grad
        out._backward = _backward
        return out

    def __mul__(self, other):
        out = Value(self.data * other.data, (self, other))
        def _backward():
            self.grad += other.data * out.grad
            other.grad += self.data * out.grad
        out._backward = _backward
        return out

    def sin(self):
        out = Value(math.sin(self.data), (self,))
        def _backward():
            self.grad += math.cos(self.data) * out.grad
        out._backward = _backward
        return out

    def backward(self):
        # Visit the values from the output back to the inputs.
        # 出力から入力へ向かって、値を順にたどる
        order, seen = [], set()
        def visit(v):
            if id(v) not in seen:
                seen.add(id(v))
                for p in v._parents:
                    visit(p)
                order.append(v)
        visit(self)
        self.grad = 1.0
        for v in reversed(order):
            v._backward()

x = Value(2.0)
y = Value(3.0)
out = x * y + x.sin()
out.backward()
print(out.data, x.grad, y.grad)    # 6.909... 2.5838... 2.0
# ANCHOR_END: micrograd

assert abs(x.grad - 2.5838531634528574) < 1e-12
assert y.grad == 2.0

# ANCHOR: torch
# Python (PyTorch)
import torch

x = torch.tensor(2.0, requires_grad=True)
y = torch.tensor(3.0, requires_grad=True)
out = x * y + torch.sin(x)
out.backward()
print(out.item(), x.grad.item(), y.grad.item())
# ANCHOR_END: torch

assert abs(x.grad.item() - 2.5838531634528574) < 1e-6
assert y.grad.item() == 2.0
