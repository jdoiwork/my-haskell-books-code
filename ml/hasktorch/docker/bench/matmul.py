# Python (PyTorch): the same matmul as app/Main.hs, with the same libtorch.
# Python（PyTorch）：app/Main.hs と同じ行列の積を、同じ libtorch で
import time
import torch

torch.set_num_threads(1)
for n in (256, 512, 1024):
    a = torch.rand(n, n, dtype=torch.float64)
    b = torch.rand(n, n, dtype=torch.float64)
    torch.matmul(a, b)  # warm up / 温める
    times = []
    for _ in range(10):
        t0 = time.perf_counter()
        torch.matmul(a, b).sum().item()  # same as asValue (sumAll ...) / 同じ形
        times.append(time.perf_counter() - t0)
    times.sort()
    print(f"pytorch n={n} median={times[5] * 1000:.2f} ms")
