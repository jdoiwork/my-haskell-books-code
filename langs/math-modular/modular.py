# Remainders in Python, for the chapter "The world of remainders".
# 「剰余の世界」の章の剰余を、Python で

# ANCHOR: functional
# Python
big = pow(3, 1_000_000, 1_000_000_007)   # 64935414, fast
inverse = pow(3, -1, 10)                 # 7: 3 * 7 = 21 = 1 (mod 10)
hour = (3 - 5) % 24                      # 22: % never goes below 0
secret = pow(65, 17, 3233)               # 2790 (tiny RSA)
back = pow(secret, 2753, 3233)           # 65
# ANCHOR_END: functional

assert big == 64935414
assert inverse == 7 and hour == 22
assert (secret, back) == (2790, 65)
try:
    pow(4, -1, 10)
    raise AssertionError("4 has no inverse mod 10")
except ValueError:
    pass
print("ok")
