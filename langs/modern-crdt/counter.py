# A counter shared by two offline devices, for the chapter "CRDTs".
# ネットワークのない2台で共有するカウンター。「CRDT」の章


# ANCHOR: procedural
# Python (procedural style: one number, add what the other device sent)
# Python（手続き型スタイル：数は1つ。ほかの端末から届いた数を足す）
class Counter:
    def __init__(self):
        self.count = 0

    def increment(self):
        self.count += 1

    def receive(self, other_count):
        self.count += other_count
# ANCHOR_END: procedural


laptop, phone = Counter(), Counter()
laptop.increment()
laptop.increment()
phone.increment()
laptop.receive(phone.count)
assert laptop.count == 3
# The same message arrives twice: the count goes wrong.
# 同じ知らせが2回届くと、数がくるう
laptop.receive(phone.count)
assert laptop.count == 4


# ANCHOR: functional
# Python (functional style: a count per device, merge with max)
# Python（関数型スタイル：端末ごとの数を持ち、max で合わせる）
def increment(counts, device):
    return {**counts, device: counts.get(device, 0) + 1}


def merge(a, b):
    return {d: max(a.get(d, 0), b.get(d, 0)) for d in a.keys() | b.keys()}


def value(counts):
    return sum(counts.values())
# ANCHOR_END: functional


laptop = increment(increment({}, "lambda"), "lambda")
phone = increment({}, "python")
merged = merge(laptop, phone)
assert value(merged) == 3
assert merge(merged, phone) == merged
assert merge(laptop, phone) == merge(phone, laptop)
print("ok")
