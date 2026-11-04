# ANCHOR: typer
# Python (Typer)
# Python（Typer）
import typer
from enum import Enum

app = typer.Typer()

class Element(str, Enum):
    FIRE = "Fire"
    WATER = "Water"
    GRASS = "Grass"

@app.command("list")
def list_monsters(element: Element | None = None,
                  sort: str = "name",
                  json: bool = False):
    ...

@app.command("add")
def add_monster(name: str, hp: int, element: Element):
    ...

app()
# ANCHOR_END: typer
