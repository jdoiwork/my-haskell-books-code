# The Python code in the Map chapter.
# Map の章の Python のコード
bag = {"potion": 3, "herb": 1, "elixir": 0}
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
if "potion" in bag:
    bag["potion"] += 2
else:
    bag["potion"] = 2
# ANCHOR_END: procedural
assert bag["potion"] == 5
bag = {"potion": 3, "herb": 1, "elixir": 0}
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
new_bag = {**bag, "potion": bag.get("potion", 0) + 2}
# ANCHOR_END: functional
assert new_bag["potion"] == 5 and bag["potion"] == 3
battle_log = [(1, "Goblin-A"), (2, "Goblin-C"), (1, "Goblin-B"), (3, "Goblin-D"), (3, "Goblin-E")]
# ANCHOR: procedural_2
# Python (procedural style)
# Python（手続き型スタイル）
from collections import defaultdict

by_turn = defaultdict(list)
for turn, name in battle_log:
    by_turn[turn].append(name)
# ANCHOR_END: procedural_2
assert dict(by_turn) == {1: ["Goblin-A", "Goblin-B"], 2: ["Goblin-C"], 3: ["Goblin-D", "Goblin-E"]}
