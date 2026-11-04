# ANCHOR: typer
# Python (Typer)
# Python（Typer）
import typer
from enum import Enum

class Element(str, Enum):
    FIRE = "Fire"
    WATER = "Water"
    GRASS = "Grass"

def main(file: str = "monsters.json",
         element: Element | None = None,
         sort: str = "name",
         json: bool = False):
    ...

typer.run(main)
# ANCHOR_END: typer
