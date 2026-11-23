# ANCHOR: all
# The answer of step 1 of the training: Time.format, for the runtime of
# Python. Put it in with PRIMS.update, and the runtime file stays as it
# is.
# トレーニングのステップ1の解答：Python のランタイムの Time.format。
# PRIMS.update で入れれば、ランタイムのファイルは変えなくてよい
import datetime


# ANCHOR: format
# 🕰️ Time.format zone t: the clock on the wall of the zone, and the
#    zone: "2026-11-25 08:30:00 +09:00". Python's datetime knows a
#    zone of a fixed offset (datetime.timezone), and strftime writes it.
# ゾーンの壁の時計と、ゾーン。Python の datetime は、ずれの決まった
# ゾーン（datetime.timezone）を知っていて、strftime が書く
def time_format(zone, t):
    minutes = zone.args[0]
    here = datetime.timezone(datetime.timedelta(minutes=minutes))
    epoch = datetime.datetime(1970, 1, 1, tzinfo=datetime.timezone.utc)
    moment = epoch + datetime.timedelta(milliseconds=t.args[0])
    local = moment.astimezone(here)
    sign = "-" if minutes < 0 else "+"
    offset = "%s%02d:%02d" % (sign, abs(minutes) // 60, abs(minutes) % 60)
    return local.strftime("%Y-%m-%d %H:%M:%S") + " " + offset


MORE_PRIMS = {
    "Time.format": (2, time_format),
}
# ANCHOR_END: format
# ANCHOR_END: all
