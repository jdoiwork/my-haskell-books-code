# ANCHOR: level
# Python (Typer, with an Enum)
# Python（Typer と Enum）
from enum import Enum
from typing import Annotated
import typer


class Level(str, Enum):
    quiet = "quiet"
    normal = "normal"
    loud = "loud"


def main(
    # What to remember / 覚えておくこと
    what: Annotated[str, typer.Argument(help="What to remember")],
    # How loud / どのくらいの声で
    level: Annotated[Level, typer.Option(
        "--level", "-l", case_sensitive=False)] = Level.normal,
):
    """Say what to remember."""
    if level == Level.quiet:
        print(f"({what})")
    elif level == Level.loud:
        print(f"{what.upper()}!")
    else:
        print(what)


if __name__ == "__main__":
    typer.run(main)
# ANCHOR_END: level
