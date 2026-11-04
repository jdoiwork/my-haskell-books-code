# The Python code in the custom type classes chapter.
# 型クラスを作る章の Python のコード
# ANCHOR: protocol
# Python (Protocol)
# Python（Protocol を使う）
from typing import Protocol

class Describable(Protocol):
    def describe(self) -> str: ...

def announce(x: Describable) -> str:
    return "Look! " + x.describe()
# ANCHOR_END: protocol
class Yes:
    def describe(self) -> str:
        return "yes, indeed"

assert announce(Yes()) == "Look! yes, indeed"
