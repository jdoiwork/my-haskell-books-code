# The guild's post office in Python, for the chapter
# "Design: a message broker as types".
# 「設計：メッセージブローカーを型で描く」の章の、ギルドの伝令所を Python で
from collections import deque

morning = [
    ("dragon", "A red dragon at the north gate"),
    ("herb", "Ten moon herbs, please"),
    ("goblin", "Goblins in the cellar"),
    ("dragon", "A baby dragon is lost"),
]

# ANCHOR: procedural
# Python (procedural style: a list of bindings, a dict of lists)
# Python（手続き型スタイル：結び付けのリストと、リストの dict）
bindings = [("dragon", "dragon-team"), ("herb", "herb-team")]
queues = {"dragon-team": [], "herb-team": []}


# 📮 Put one letter into every mailbox bound to its key.
# 手紙を1通、そのキーに結ばれた郵便受けすべてに入れる
def publish(letter):
    key, body = letter
    sent = []
    for binding_key, queue in bindings:
        if binding_key == key and queue not in sent:
            queues[queue].append(body)
            sent.append(queue)
    return sent  # [] means nowhere to go / [] は行き先なし
# ANCHOR_END: procedural


results = [publish(letter) for letter in morning]
assert results == [["dragon-team"], ["herb-team"], [], ["dragon-team"]]
assert queues["dragon-team"] == [
    "A red dragon at the north gate",
    "A baby dragon is lost",
]
assert queues["herb-team"] == ["Ten moon herbs, please"]


# ANCHOR: functional
# Python (functional style: a comprehension, and deque for the queues)
# Python（関数型スタイル：内包表記と、キューには deque）
bindings = [("dragon", "dragon-team"), ("herb", "herb-team")]
queues = {"dragon-team": deque(), "herb-team": deque()}


# 📮 Find the targets first, then deliver.
# まず行き先を決めてから、届ける
def publish(letter):
    key, body = letter
    targets = list(dict.fromkeys(
        queue for binding_key, queue in bindings if binding_key == key
    ))
    for queue in targets:
        queues[queue].append(body)
    return targets
# ANCHOR_END: functional


results = [publish(letter) for letter in morning]
assert results == [["dragon-team"], ["herb-team"], [], ["dragon-team"]]
assert queues["dragon-team"].popleft() == "A red dragon at the north gate"
assert queues["dragon-team"].popleft() == "A baby dragon is lost"
assert list(queues["herb-team"]) == ["Ten moon herbs, please"]
# A duplicate binding still delivers once.
# 同じ結び付けが2つあっても、届くのは1回
bindings.append(("herb", "herb-team"))
assert publish(("herb", "More herbs")) == ["herb-team"]
assert list(queues["herb-team"]) == ["Ten moon herbs, please", "More herbs"]
print("ok")
