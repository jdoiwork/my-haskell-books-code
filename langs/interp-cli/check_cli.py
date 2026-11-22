# Give the same words to the greeting CLI of each language, and check
# the answers, the help and the mistakes.
# 各言語のあいさつの CLI に同じ語を渡し、答え・ヘルプ・まちがいを確かめる
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
run = os.path.join(here, "run")
cargo_bin = os.environ.get("BOOK_CARGO_BIN", "")
# A narrow terminal, and no colors (Typer draws boxes with rich).
# 狭い端末で、色なし（Typer は rich で枠を描く）
env = dict(os.environ, COLUMNS="78", NO_COLOR="1", TERM="dumb")


def call(cmd):
    return subprocess.run(cmd, capture_output=True, text=True, env=env)


def py(name):
    return [sys.executable, os.path.join(run, name)]


programs = [py("typer_plain.py"), py("typer_greet.py"),
            py("argparse_greet.py")]
if cargo_bin:
    programs.append([os.path.join(cargo_bin, "interp_cli")])

for p in programs:
    # The same answers / 同じ答え
    got = call(p + ["Lam", "--count", "2", "--loud"])
    assert got.returncode == 0, (p, got)
    assert got.stdout == "HELLO, LAM!\nHELLO, LAM!\n", (p, got)
    got = call(p + ["Lam"])
    assert got.stdout == "Hello, Lam!\n", (p, got)
    # A missing argument and a wrong number stop with code 2, like Lam
    # 引数がない・数でないと、Lam と同じく終了コード 2 で止まる
    for words in [[], ["Lam", "--count", "abc"]]:
        got = call(p + words)
        assert got.returncode == 2 and got.stdout == "", (p, words, got)
    # The comment (the docstring, ///) is in the help
    # コメント（docstring、///）がヘルプに出る
    got = call(p + ["--help"])
    assert got.returncode == 0 and "Greet someone" in got.stdout, (p, got)

# Typer makes --loud and --no-loud from a bool, and shows the default
# Typer は bool から --loud と --no-loud を作り、既定値を見せる
help_text = call(py("typer_plain.py") + ["--help"]).stdout
assert "--no-loud" in help_text and "[default: 1]" in help_text, help_text
# The short -n of typer_greet.py and argparse
# typer_greet.py と argparse の短い -n
for p in [py("typer_greet.py"), py("argparse_greet.py")]:
    assert call(p + ["Lam", "-n", "2"]).stdout == "Hello, Lam!\nHello, Lam!\n"
# Typer and clap say which option was meant
# Typer と clap は、どのオプションのつもりかを教える
got = call(py("typer_plain.py") + ["Lam", "--cont", "2"])
assert "Possible options: --count" in got.stderr, got.stderr
if cargo_bin:
    got = call([os.path.join(cargo_bin, "interp_cli"), "Lam", "--cont", "2"])
    assert "a similar argument exists: '--count'" in got.stderr, got.stderr
# A type of its own: Typer's Enum and clap's FromStr, like FromArg
# 自分の型：Typer の Enum と clap の FromStr（FromArg と同じ）
levels = [py("typer_level.py")]
if cargo_bin:
    levels.append([os.path.join(cargo_bin, "interp_cli_level")])
for p in levels:
    assert call(p + ["milk"]).stdout == "milk\n", p
    assert call(p + ["milk", "-l", "LOUD"]).stdout == "MILK!\n", p
    assert call(p + ["milk", "--level", "quiet"]).stdout == "(milk)\n", p
    got = call(p + ["milk", "--level", "shout"])
    assert got.returncode == 2 and got.stdout == "", (p, got)
got = call([os.path.join(cargo_bin, "interp_cli_level"), "milk", "-l", "x"]) \
    if cargo_bin else None
if got:
    assert "quiet, normal or loud" in got.stderr, got.stderr
print("ok")
