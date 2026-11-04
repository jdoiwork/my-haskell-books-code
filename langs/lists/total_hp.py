# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def total_hp(hps):
    total = 0
    for hp in hps:
        total += hp
    return total
# ANCHOR_END: procedural
assert total_hp([30, 5, 80, 12, 45]) == 172 and total_hp([]) == 0
