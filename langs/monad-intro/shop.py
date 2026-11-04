# The Python code in the Monad intro chapter.
# Monad 入門の章の Python のコード
PRICES = {"potion": 50, "elixir": 150, "sword": 300}


def price(item):
    return PRICES.get(item)


def pay(cost, gold):
    return gold - cost if cost <= gold else None


def buy(item, gold):
    p = price(item)
    if p is None:
        return None
    return pay(p, gold)


# ANCHOR: procedural
# Python (procedural style: check for None after every step)
# Python（手続き型スタイル：1歩ごとに None を確かめる）
def buy_three(a, b, c, gold):
    g1 = buy(a, gold)
    if g1 is None:
        return None
    g2 = buy(b, g1)
    if g2 is None:
        return None
    return buy(c, g2)
# ANCHOR_END: procedural

assert buy_three("potion", "elixir", "potion", 300) == 50
assert buy_three("potion", "sword", "potion", 300) is None
assert buy_three("potion", "bow", "potion", 300) is None
print("ok")
