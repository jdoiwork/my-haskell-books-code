# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
n, x = map(int, input().split())
coins = list(map(int, input().split()))
found = False
for mask in range(1 << n):
    total = 0
    for i in range(n):
        if mask >> i & 1:
            total += coins[i]
    if total == x:
        found = True
        break
print("Yes" if found else "No")
# ANCHOR_END: procedural
