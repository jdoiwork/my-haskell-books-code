# A dict with deadlines in Python, for the chapter "Expiry and clocks".
# The clock is passed in, so a test can move time by hand.
# 「期限切れと時計」の章の、期限つきの辞書を Python で。
# 時計を外から渡すので、テストでは手で時間を進められる
import time


# ANCHOR: procedural
# Python (procedural style: the clock is an argument)
# Python（手続き型スタイル：時計は引数）
class TtlStore:
    def __init__(self, clock=time.monotonic):
        self.clock = clock
        self.values = {}
        self.deadlines = {}

    def set(self, key, value, seconds=None):
        self.values[key] = value
        self.deadlines.pop(key, None)
        if seconds is not None:
            self.deadlines[key] = self.clock() + seconds

    def get(self, key):
        deadline = self.deadlines.get(key)
        if deadline is not None and deadline <= self.clock():
            del self.values[key], self.deadlines[key]   # lazy / 遅れて消す
        return self.values.get(key)
# ANCHOR_END: procedural


now = [0.0]                       # a clock we move by hand / 手で進める時計
store = TtlStore(clock=lambda: now[0])
store.set("session", "abc", seconds=10)
now[0] = 9.999
assert store.get("session") == "abc"
now[0] = 10.0
assert store.get("session") is None
print("ok")
