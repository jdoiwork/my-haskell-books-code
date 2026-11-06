# The fast inverse square root of the chapter "Numbers", in Python.
# 「数の正体」の章の高速逆平方根を、Python で
import math
import struct


# ANCHOR: functional
# Python
def fast_inv_sqrt(x):
    # Pack the float into 4 bytes, and read them as an unsigned int.
    # float を4バイトに詰めて、符号なし整数として読む
    (bits,) = struct.unpack("<I", struct.pack("<f", x))
    bits = 0x5F3759DF - (bits >> 1)
    (guess,) = struct.unpack("<f", struct.pack("<I", bits))
    return guess * (1.5 - 0.5 * x * guess * guess)
# ANCHOR_END: functional


for x in [0.25, 2.0, 100.0, 12345.0]:
    assert abs(fast_inv_sqrt(x) * math.sqrt(x) - 1) < 0.002
print("ok")
