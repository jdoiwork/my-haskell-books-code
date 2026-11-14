# The Python code in the search engine CLI chapter.
# 検索エンジンの CLI の章の Python のコード
import json

# ANCHOR: argparse
# Python (argparse: subcommands)
# Python（argparse：サブコマンド）
import argparse

def positive_int(s):
    # int() also reads " 7", "+7", "1_0" and full-width digits.
    # int() は " 7"・"+7"・"1_0"・全角の数字も読むので、先に確かめる
    if not (s.isascii() and s.isdigit()):
        raise argparse.ArgumentTypeError(f"not a positive number: {s}")
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
for bad in ["0", "0x5", " 7", "+7", "1_0", "１２"]:
    try:
        positive_int(bad)
        raise AssertionError(bad)
    except argparse.ArgumentTypeError:
        pass
# No upper limit: Python's int has no end to wrap around.
# 上限はない：Python の int には、一周する端がない
assert positive_int("18446744073709551617") == 18446744073709551617

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
