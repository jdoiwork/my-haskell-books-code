# The Python code in the "IO exceptions" chapter.
# IO の例外をちゃんと扱うの章の Python のコード
from contextlib import contextmanager

log = []


class GoblinAlarm(Exception):
    pass


def open_door():
    log.append("door: open")


def close_door():
    log.append("door: closed")


def count_coins():
    log.append("counting: 1, 2, 3, ...")
    raise GoblinAlarm()


# ANCHOR: finally
# Python (procedural style: try / finally)
# Python（手続き型スタイル：try / finally）
def shift(work):
    open_door()
    try:
        work()
    finally:
        close_door()
# ANCHOR_END: finally


try:
    shift(count_coins)
except GoblinAlarm:
    pass
assert log == ["door: open", "counting: 1, 2, 3, ...", "door: closed"], log
log.clear()


# ANCHOR: with
# Python (a context manager: the with statement)
# Python（コンテキストマネージャー：with 文）
@contextmanager
def in_vault():
    open_door()
    try:
        yield
    finally:
        close_door()


def night_shift():
    with in_vault():
        count_coins()
# ANCHOR_END: with


try:
    night_shift()
except GoblinAlarm:
    pass
assert log == ["door: open", "counting: 1, 2, 3, ...", "door: closed"], log
log.clear()


# ANCHOR: catch_all
# Python (except Exception does not catch Ctrl-C:
#         KeyboardInterrupt is a BaseException, not an Exception)
# Python（except Exception は Ctrl-C をつかまえない。
#         KeyboardInterrupt は BaseException の子で、Exception の子ではない）
def appraise_stubborn(i):
    try:
        log.append(f"appraising #{i}")
        if i == 2:
            raise KeyboardInterrupt  # as if Ctrl-C came / Ctrl-C が来たつもり
    except Exception as error:
        log.append(f"  shrugged off: {error!r}")
# ANCHOR_END: catch_all


try:
    for i in range(1, 11):
        appraise_stubborn(i)
except KeyboardInterrupt:
    log.append("stopped")
assert log == ["appraising #1", "appraising #2", "stopped"], log
assert not issubclass(KeyboardInterrupt, Exception)
assert not issubclass(SystemExit, Exception)
assert issubclass(KeyboardInterrupt, BaseException)
print("ok")
