# ANCHOR: argparse
# Python (argparse)
# Python（argparse）
import argparse

parser = argparse.ArgumentParser(prog="monlist")
parser.add_argument("-f", "--file", default="monsters.json")
parser.add_argument("-e", "--element",
                    choices=["Fire", "Water", "Grass"])
parser.add_argument("--sort", choices=["name", "hp"], default="name")
parser.add_argument("--json", action="store_true")

args = parser.parse_args()
# ANCHOR_END: argparse
print(sorted(vars(args).items()))
