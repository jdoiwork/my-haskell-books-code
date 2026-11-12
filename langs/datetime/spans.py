# The Python code in the "dates and times" chapter.
# 日付と時刻の章の Python のコード
import calendar
from datetime import date, datetime, timedelta

posted = datetime(2026, 11, 13, 9, 0)

# ANCHOR: timedelta
# Python (a timedelta: a length of time, in days and seconds)
# Python（timedelta：日と秒で数える、時間の長さ）
limit = posted + timedelta(hours=72)         # 2026-11-16 09:00
left = limit - (posted + timedelta(hours=45, minutes=54, seconds=53))
shown = str(left)                            # "1 day, 2:05:07"
hours = left.total_seconds() / 3600          # 26.085...
# ANCHOR_END: timedelta

assert limit == datetime(2026, 11, 16, 9, 0)
assert shown == "1 day, 2:05:07"
assert int(hours) == 26


# ANCHOR: months
# Python (procedural style: timedelta has no months, so clip by hand)
# Python（手続き型スタイル：timedelta に月はないので、自分で切り詰める）
def add_months_clip(day, n):
    month_index = day.month - 1 + n
    year = day.year + month_index // 12
    month = month_index % 12 + 1
    last = calendar.monthrange(year, month)[1]
    return date(year, month, min(day.day, last))
# ANCHOR_END: months


assert add_months_clip(date(2027, 1, 31), 1) == date(2027, 2, 28)
assert add_months_clip(date(2027, 1, 31), 13) == date(2028, 2, 29)

# ANCHOR: replace
# Python (just changing the month fails on a missing day)
# Python（月だけを変えると、ない日で止まる）
try:
    date(2027, 1, 31).replace(month=2)
except ValueError as error:
    message = str(error)        # day 31 must be in range 1..28 for month 2 ...
# ANCHOR_END: replace

assert message.startswith("day 31 must be in range 1..28 for month 2")

# ANCHOR: functional
# Python (functional style: every pay day, counted from the first one)
# Python（関数型スタイル：どの支払日も、最初の日から数える）
first = date(2027, 1, 31)
pay_days = [add_months_clip(first, n) for n in range(4)]
# ANCHOR_END: functional

assert pay_days == [date(2027, 1, 31), date(2027, 2, 28),
                    date(2027, 3, 31), date(2027, 4, 30)]
print("ok")
