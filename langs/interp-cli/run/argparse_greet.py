# ANCHOR: argparse
# Python (argparse)
# Python（argparse）
import argparse

parser = argparse.ArgumentParser(
    description="Greet someone, as many times as you like.")
parser.add_argument("name", help="Who to greet")
parser.add_argument("-n", "--count", type=int, default=1,
                    help="How many times")
parser.add_argument("--loud", action="store_true", help="Shout it")
args = parser.parse_args()

line = f"Hello, {args.name}!"
for _ in range(args.count):
    print(line.upper() if args.loud else line)
# ANCHOR_END: argparse
