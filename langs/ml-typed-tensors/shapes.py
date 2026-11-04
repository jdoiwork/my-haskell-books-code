# The shape mistakes of the chapter "Keeping dimensions in types",
# in NumPy and PyTorch: they are found only when the code runs.
# 「型で次元を守る」の章の形のまちがいを、NumPy と PyTorch で。
# 見つかるのは、動かしたときだけ
import numpy as np
import torch
from torch import nn

# ANCHOR: numpy
# Python (NumPy)
net_weight = np.zeros((35, 32))
x = np.zeros((4, 36))  # 36 columns by mistake / まちがえて 36 列

try:
    hidden = x @ net_weight
except ValueError as e:
    print(e)
# ANCHOR_END: numpy
    numpy_message = str(e)
assert "mismatch in its core dimension 0" in numpy_message, numpy_message

# ANCHOR: torch
# Python (PyTorch)
# The second layer takes 16, but the first gives 32.
# 1つ目の層は 32 個を出すのに、2つ目の層は 16 個を受け取る
model = nn.Sequential(nn.Linear(35, 32), nn.ReLU(), nn.Linear(16, 10))
print("built:", model[2])   # no error yet / まだエラーにならない

try:
    model(torch.zeros(4, 35))
except RuntimeError as e:
    print(e)
# ANCHOR_END: torch
    torch_message = str(e)
assert torch_message == (
    "addmm: mat1 and mat2 shapes cannot be multiplied (4x32 and 16x10)"
), torch_message
print("ok")
