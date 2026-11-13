# The Python code in the "time zones and summer time" chapter.
# タイムゾーンと夏時間の章の Python のコード
from datetime import date, datetime, timedelta, timezone
from zoneinfo import ZoneInfo

# ANCHOR: naive
# Python (procedural style: a naive datetime has no time zone)
# Python（手続き型スタイル：naive な datetime は時間帯を持たない）
posted = datetime(2026, 11, 12, 21, 0)       # naive: which 21:00?
today = posted.date()                        # 2026-11-12
due = today + timedelta(days=7)              # 2026-11-19
# ANCHOR_END: naive

assert posted.tzinfo is None
assert today == date(2026, 11, 12)
assert due == date(2026, 11, 19)

# ANCHOR: aware
# Python (aware datetimes: the moment, and the wall clock of Tokyo)
# Python（aware な datetime：UTC の日時と、東京の壁の時計）
posted = datetime(2026, 11, 12, 21, 0, tzinfo=timezone.utc)
tokyo = posted.astimezone(ZoneInfo("Asia/Tokyo"))
today = tokyo.date()                         # 2026-11-13
due = today + timedelta(days=7)              # 2026-11-20
# ANCHOR_END: aware

assert str(tokyo) == "2026-11-13 06:00:00+09:00"
assert today == date(2026, 11, 13)
assert due == date(2026, 11, 20)

# ANCHOR: branches
# Python (functional style: every branch's wall clock in one go)
# Python（関数型スタイル：全部の支部の壁の時計を、まとめて）
branches = ["Asia/Tokyo", "America/New_York", "Europe/London"]
walls = [posted.astimezone(ZoneInfo(name)).strftime("%Y-%m-%d %H:%M %Z")
         for name in branches]
# ANCHOR_END: branches

assert walls == [
    "2026-11-13 06:00 JST",
    "2026-11-12 16:00 EST",
    "2026-11-12 21:00 GMT",
]

# ANCHOR: mixed
# Python (naive and aware cannot be compared: an error at run time)
# Python（naive と aware はくらべられない。動かしたときのエラー）
try:
    datetime(2026, 11, 12, 21, 0) < posted
except TypeError as error:
    message = str(error)   # can't compare offset-naive and offset-aware ...
# ANCHOR_END: mixed

assert message.startswith("can't compare offset-naive and offset-aware")

# ANCHOR: iso
# Python (ISO 8601: write and read back)
# Python（ISO 8601：書いて、読み戻す）
text = posted.isoformat()                    # "2026-11-12T21:00:00+00:00"
back = datetime.fromisoformat(text)
# ANCHOR_END: iso

assert text == "2026-11-12T21:00:00+00:00"
assert back == posted
assert datetime.fromisoformat("2026-11-12T21:00:00Z") == posted
print("ok")
