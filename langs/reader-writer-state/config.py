# Passing settings around, for the chapter "Reader, Writer and State".
# 設定を持ち回る。「Reader・Writer・State」の章
from dataclasses import dataclass


@dataclass
class Config:
    player_name: str
    difficulty: int


# ANCHOR: procedural
# Python (procedural style: pass the config to every function)
# Python（手続き型スタイル：どの関数にも設定を渡す）
def greeting(cfg):
    return f"Welcome, {cfg.player_name}!"


def enemy_hp(cfg, base):
    return base * cfg.difficulty


def floor_report(cfg, floor_no):
    hp = enemy_hp(cfg, floor_no * 10)
    return f"{greeting(cfg)} Floor {floor_no}: goblin with {hp} HP"
# ANCHOR_END: procedural


# ANCHOR: object
# Python (an object holds the config: dependency injection)
# Python（オブジェクトが設定を持つ：依存性の注入）
class Dungeon:
    def __init__(self, cfg):
        self.cfg = cfg

    def greeting(self):
        return f"Welcome, {self.cfg.player_name}!"

    def enemy_hp(self, base):
        return base * self.cfg.difficulty

    def floor_report(self, floor_no):
        hp = self.enemy_hp(floor_no * 10)
        return f"{self.greeting()} Floor {floor_no}: goblin with {hp} HP"
# ANCHOR_END: object


easy = Config("Lambda", 1)
expected = "Welcome, Lambda! Floor 3: goblin with 30 HP"
assert floor_report(easy, 3) == expected
assert Dungeon(easy).floor_report(3) == expected
print("ok")
