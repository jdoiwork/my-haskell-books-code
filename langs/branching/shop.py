# The Python code in the branching chapter.
# 場合分けと Maybe の章の Python のコード
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def category(item):
    if item == "potion" or item == "elixir":
        return "medicine"
    elif item == "sword":
        return "weapon"
    elif item == "shield":
        return "armor"
    else:
        return "unknown"
# ANCHOR_END: procedural
assert [category(i) for i in ["elixir", "sword", "shield", "bow"]] == ["medicine", "weapon", "armor", "unknown"]
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
CATEGORIES = {"potion": "medicine", "elixir": "medicine",
              "sword": "weapon", "shield": "armor"}

def category(item):
    return CATEGORIES.get(item, "unknown")
# ANCHOR_END: functional
assert [category(i) for i in ["potion", "sword", "bow"]] == ["medicine", "weapon", "unknown"]
# ANCHOR: procedural_2
# Python (procedural style)
# Python（手続き型スタイル）
def price(item):
    if item == "potion":
        return 50
    elif item == "elixir":
        return 150
    elif item == "sword":
        return 300
    return None
# ANCHOR_END: procedural_2
assert price("potion") == 50 and price("bow") is None
# ANCHOR: procedural_3
# Python (procedural style)
# Python（手続き型スタイル）
def price_tag(item):
    p = price(item)
    if p is None:
        return f"{item}: not for sale"
    return f"{item}: {p} G"
# ANCHOR_END: procedural_3
assert price_tag("sword") == "sword: 300 G" and price_tag("bow") == "bow: not for sale"
# ANCHOR: functional_2
# Python (functional style)
# Python（関数型スタイル）
PRICES = {"potion": 50, "elixir": 150, "sword": 300}

def price_tag(item):
    p = PRICES.get(item)
    return f"{item}: not for sale" if p is None else f"{item}: {p} G"
# ANCHOR_END: functional_2
assert price_tag("sword") == "sword: 300 G" and price_tag("bow") == "bow: not for sale"
