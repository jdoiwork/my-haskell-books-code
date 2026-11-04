# ANCHOR: argparse
# Python (argparse)
# Python（argparse）
import argparse

parser = argparse.ArgumentParser(prog="mondex")
parser.add_argument("-f", "--file", default="monsters.json")
commands = parser.add_subparsers(dest="command", required=True)

list_cmd = commands.add_parser("list", help="List the monsters")
list_cmd.add_argument("-e", "--element",
                      choices=["Fire", "Water", "Grass"])
list_cmd.add_argument("--sort", choices=["name", "hp"],
                      default="name")
list_cmd.add_argument("--json", action="store_true")

args = parser.parse_args()
# ANCHOR_END: argparse
print(sorted(vars(args).items()))
