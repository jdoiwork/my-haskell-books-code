# The Python code in the JSON chapter.
# JSON を扱う章の Python のコード
# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
import json

def load_monster(text):
    data = json.loads(text)
    if "hp" not in data:
        raise ValueError("hp is missing")
    if not isinstance(data["hp"], int):
        raise ValueError("hp must be a number")
    if data["element"] not in ("Fire", "Water", "Grass"):
        raise ValueError("unknown element")
    # ... name and drops, too
    return data
# ANCHOR_END: procedural
assert load_monster('{"name": "Slime", "hp": 30, "element": "Water", "drops": ["gel"]}')['hp'] == 30
for bad in ('{"name": "Slime", "element": "Water"}',
            '{"name": "Slime", "hp": "30", "element": "Water"}',
            '{"name": "Slime", "hp": 30, "element": "Fier"}'):
    try:
        load_monster(bad)
        raise AssertionError("should fail: " + bad)
    except ValueError:
        pass
text = '{"name": "Slime", "hp": 30, "element": "Water", "drops": ["gel"]}'
# ANCHOR: pydantic
# Python (pydantic)
# Python（pydantic を使う）
from enum import Enum
from pydantic import BaseModel

class Element(str, Enum):
    FIRE = "Fire"
    WATER = "Water"
    GRASS = "Grass"

class Monster(BaseModel):
    name: str
    hp: int
    element: Element
    drops: list[str]

monster = Monster.model_validate_json(text)
# ANCHOR_END: pydantic
assert monster.hp == 30 and monster.element is Element.WATER and monster.drops == ["gel"]
