# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
n, k = map(int, input().split())
total = 0
for box in range(1, n + 1):
    digit_sum = 0
    rest = box
    while rest > 0:
        digit_sum += rest % 10
        rest //= 10
    if digit_sum == k:
        total += box
print(total)
# ANCHOR_END: procedural
