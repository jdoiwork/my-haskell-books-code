# The Python code in the chapter on ? and cleaning up: a failure is an
# exception, that flies up by itself; finally and with clean up.
# ? と後始末の章の Python のコード：失敗は例外で、ひとりでに上へ飛ぶ。
# 後始末は finally と with
from contextlib import contextmanager


# ANCHOR: procedural
# Python (procedural style): try and finally
# Python（手続き型スタイル）：try と finally
def total(read, log):
    log.append("the shop opens")
    try:
        text = read("orders.txt")    # an OSError flies up from here
        sum_ = 0
        for word in text.split():
            sum_ += int(word)        # a ValueError flies up from here
        return sum_
    finally:
        log.append("the shop closes")
# ANCHOR_END: procedural


# ANCHOR: with
# Python (with): the cleaning up is the job of the shop itself
# Python（with）：後始末は、お店そのものの仕事
@contextmanager
def shop(log):
    log.append("the shop opens")
    try:
        yield
    finally:
        log.append("the shop closes")


def total_with(read, log):
    with shop(log):
        return sum(int(word) for word in read("orders.txt").split())
# ANCHOR_END: with


def read(name):
    if name == "orders.txt":
        return "4 1 3"
    raise FileNotFoundError(f"{name}: no such file")


def missing(name):
    raise FileNotFoundError(f"{name}: no such file")


for f in [total, total_with]:
    log = []
    assert f(read, log) == 8
    try:
        f(missing, log)
        assert False
    except FileNotFoundError as e:
        assert str(e) == "orders.txt: no such file"
    try:
        f(lambda _: "4 one", log)
        assert False
    except ValueError as e:
        assert "invalid literal for int()" in str(e)
    assert log == ["the shop opens", "the shop closes"] * 3, log
print("ok receipt.py")
