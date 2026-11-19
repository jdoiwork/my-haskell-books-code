# The Python code in the chapter on Map and JSON: keys that are pairs.
# Map と JSON の章の Python のコード：組をキーにする
moves = ["R", "R", "D", "L", "U", "U"]
steps = {"R": (1, 0), "L": (-1, 0), "D": (0, 1), "U": (0, -1)}
treasures = {(2, 0): "herb", (1, 1): "elixir", (5, 5): "key"}

# ANCHOR: procedural
# Python (procedural style): a tuple can be a key of a dict, and a
# value of a set
# Python（手続き型スタイル）：タプルは、dict のキーにも set の値にもなる
x, y = 0, 0
visited = {(x, y)}
for m in moves:
    dx, dy = steps[m]
    x, y = x + dx, y + dy
    visited.add((x, y))
found = []
for cell, item in sorted(treasures.items()):
    if cell in visited:
        found.append(item)
print(len(visited), found)  # 6 ['elixir', 'herb']
# ANCHOR_END: procedural

assert len(visited) == 6
assert found == ["elixir", "herb"]

# ANCHOR: functional
# Python (functional style): the same with comprehensions. A list
# cannot be a key: it can change, so it has no hash.
# Python（関数型スタイル）：内包表記で。リストはキーにできない。
# 書き換えられるので、ハッシュを持たない
found2 = [item for cell, item in sorted(treasures.items())
          if cell in visited]
try:
    {[1, 2]: "a"}
except TypeError as e:
    print(e)  # cannot use 'list' as a dict key (unhashable type: 'list')
# ANCHOR_END: functional

assert found2 == found
try:
    {[1, 2]: "a"}
    raise AssertionError("a list became a key")
except TypeError as e:
    assert "unhashable" in str(e)
