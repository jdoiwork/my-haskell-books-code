# The Python code in the chapter on type annotations and records.
# 型の注釈とレコードの章の Python のコード
from dataclasses import dataclass, replace
from typing import Protocol

# ANCHOR: dataclass
# Python (a frozen dataclass, and replace)
# Python（書き換えられないデータクラスと、replace）
@dataclass(frozen=True)
class Member:
    name: str
    lang: str
    days: int

def next_day(m: Member) -> Member:
    # A new one with days changed; m stays as it was.
    # days だけ変えた新しいもの。m はそのまま
    return replace(m, days=m.days + 1)
# ANCHOR_END: dataclass

# ANCHOR: protocol
# Python (a Protocol: anything with a name, for the type checker)
# Python（Protocol：name を持つものなら何でも。型チェッカーのため）
class Named(Protocol):
    @property
    def name(self) -> str: ...

def label(r: Named) -> str:
    return "[" + r.name + "]"
# ANCHOR_END: protocol

@dataclass
class Lang:
    name: str
    version: str

lam = Member("Lam", "lam", 9)
assert next_day(lam).days == 10 and lam.days == 9
assert label(next_day(lam)) == "[Lam]"
assert label(Lang("Ruby", "4.0")) == "[Ruby]"
print(label(next_day(lam)), label(Lang("Ruby", "4.0")))
