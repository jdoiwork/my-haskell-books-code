# The Python code in the search engine CLI chapter.
# 検索エンジンの CLI の章の Python のコード
import json

# ANCHOR: argparse
# Python (argparse: subcommands)
# Python（argparse：サブコマンド）
import argparse

def positive_int(s):
    n = int(s)
    if n < 1:
        raise argparse.ArgumentTypeError(f"not a positive number: {s}")
    return n

def build_parser():
    parser = argparse.ArgumentParser(
        prog="minisearch",
        description="A mini full-text search engine")
    sub = parser.add_subparsers(dest="command", required=True)

    index = sub.add_parser("index", help="Build an index")
    index.add_argument("dir")
    index.add_argument("-o", "--out", default="index.json")

    search = sub.add_parser("search", help="Search the index")
    search.add_argument("-i", "--index", default="index.json")
    search.add_argument("-n", "--limit", type=positive_int, default=5)
    search.add_argument("words", nargs="+")
    return parser
# ANCHOR_END: argparse

args = build_parser().parse_args(["search", "-n", "3", "red", "dragon"])
assert args.command == "search"
assert args.limit == 3
assert " ".join(args.words) == "red dragon"
assert args.index == "index.json"

# ANCHOR: keys
# Python (JSON keys are always strings)
# Python（JSON のキーは、いつも文字列）
postings = {"dragon": {1: 2, 3: 2}}
text = json.dumps(postings)
back = json.loads(text)
assert text == '{"dragon": {"1": 2, "3": 2}}'
assert back["dragon"] == {"1": 2, "3": 2}   # not {1: 2, 3: 2}
# ANCHOR_END: keys
print("ok")
