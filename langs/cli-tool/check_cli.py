# Give the same options to the CLI code of each language, and check that
# they are read. / 各言語の CLI のコードに同じオプションを渡して、読めるか確かめる
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
node = os.environ.get("BOOK_NODE", "node")
cargo_bin = os.environ.get("BOOK_CARGO_BIN", "")


def out(cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout


args = ["--element", "Fire", "--sort", "hp", "--json"]
got = out([sys.executable, os.path.join(run, "argparse_cli.py"), *args])
assert "('element', 'Fire')" in got and "('json', True)" in got and "('sort', 'hp')" in got, got
out([sys.executable, os.path.join(run, "typer_cli.py"), *args])
got = out([node, "--no-warnings", os.path.join(run, "commander_cli.ts"), *args])
assert '"element":"Fire"' in got and '"json":true' in got and '"sort":"hp"' in got, got
# a wrong element is an error / 知らない属性はエラー
bad = subprocess.run([sys.executable, os.path.join(run, "argparse_cli.py"), "--element", "Fier"],
                     capture_output=True, text=True)
assert bad.returncode != 0 and "invalid choice" in bad.stderr, bad.stderr
if cargo_bin:
    got = out([os.path.join(cargo_bin, "cli_tool"), "--element", "fire", "--json"])
    assert got.strip() == "monsters.json Some(Fire) name true", got
