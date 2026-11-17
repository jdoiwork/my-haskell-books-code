# ANCHOR: typer
# Python (Typer)
# Python（Typer）
from typing import Annotated
import typer


def main(
    # Who to greet / あいさつする相手
    name: Annotated[str, typer.Argument(help="Who to greet")],
    # How many times / 何回
    count: Annotated[int, typer.Option("--count", "-n",
                                       help="How many times")] = 1,
    # Shout it / 大声で
    loud: Annotated[bool, typer.Option(help="Shout it")] = False,
):
    """Greet someone, as many times as you like."""
    line = f"Hello, {name}!"
    for _ in range(count):
        print(line.upper() if loud else line)


if __name__ == "__main__":
    typer.run(main)
# ANCHOR_END: typer
