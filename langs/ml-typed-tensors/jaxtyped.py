# Shapes in Python type hints, with jaxtyping and beartype:
# the names "batch" and "inputs" must agree, checked when the function runs.
# Python の型ヒントに形を書く（jaxtyping と beartype）。
# "batch" や "inputs" の名前がそろっているかを、関数を呼んだときに確かめる
import torch
from beartype import beartype
from jaxtyping import Float, jaxtyped
from torch import Tensor

# ANCHOR: hints
# Python (jaxtyping)
@jaxtyped(typechecker=beartype)
def linear(
    x: Float[Tensor, "batch inputs"],
    weight: Float[Tensor, "inputs outputs"],
    bias: Float[Tensor, "outputs"],
) -> Float[Tensor, "batch outputs"]:
    return x @ weight + bias
# ANCHOR_END: hints

ok = linear(torch.zeros(4, 35), torch.zeros(35, 32), torch.zeros(32))
assert ok.shape == (4, 32)

try:
    linear(torch.zeros(4, 36), torch.zeros(35, 32), torch.zeros(32))
except Exception as e:
    message = str(e)
    print(type(e).__name__)
assert "inputs" in message, message
print("ok")
