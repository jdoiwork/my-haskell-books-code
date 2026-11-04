# Give the same subcommand to the CLI code of each language, and check that
# it is read. / 各言語の CLI のコードに同じサブコマンドを渡して、読めるか確かめる
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
node = os.environ.get("BOOK_NODE", "node")
cargo_bin = os.environ.get("BOOK_CARGO_BIN", "")


def out(cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout


got = out([sys.executable, os.path.join(run, "argparse_cli.py"),
           "-f", "my.json", "list", "--element", "Water", "--json"])
assert "('command', 'list')" in got and "('element', 'Water')" in got and "('file', 'my.json')" in got, got
out([sys.executable, os.path.join(run, "typer_cli.py"), "list", "--element", "Water"])
out([sys.executable, os.path.join(run, "typer_cli.py"), "add", "Bat", "15", "Grass"])
# without a subcommand, it is an error / サブコマンドがないとエラー
bad = subprocess.run([sys.executable, os.path.join(run, "argparse_cli.py")], capture_output=True, text=True)
assert bad.returncode != 0, bad
out([node, "--no-warnings", os.path.join(run, "commander_cli.ts"), "list", "--element", "Water", "--json"])
if cargo_bin:
    got = out([os.path.join(cargo_bin, "cli_subcommands"), "-f", "my.json", "list", "--element", "water"])
    assert got.strip() == "my.json Some(Water) false", got
