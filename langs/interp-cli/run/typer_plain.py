# ANCHOR: typer
# Python (Typer: the types and the defaults alone)
# Python（Typer：型と既定値だけ）
import typer


def main(name: str, count: int = 1, loud: bool = False):
    """Greet someone, as many times as you like."""
    line = f"Hello, {name}!"
    for _ in range(count):
        print(line.upper() if loud else line)


if __name__ == "__main__":
    typer.run(main)
# ANCHOR_END: typer
