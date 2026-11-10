# A note shown or hidden, as the API sends it: the "tag" says which.
# 見せる感想か、隠した感想か。API が送るとおりに。どちらかは "tag" が言う
import json

wire = """[
  {"tag": "ShownNote", "note": {"entry": null, "body": "絵が多くて読みやすい"}},
  {"tag": "HiddenNote", "entry": 8, "hint": "ネタバレ（「第4章」を読んだら見えます）"}
]"""
seen = json.loads(wire)


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def lines_of(notes):
    lines = []
    for s in notes:
        if s["tag"] == "ShownNote":
            lines.append(s["note"]["body"])
        else:
            lines.append(s["hint"])
    return lines
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style: match, 3.10 and later)
# Python（関数型スタイル：match。3.10 から）
def line_of(s):
    match s:
        case {"tag": "ShownNote", "note": {"body": body}}:
            return body
        case {"tag": "HiddenNote", "hint": hint}:
            return hint
        case _:
            raise ValueError(f"unknown note: {s}")


lines = [line_of(s) for s in seen]
# ANCHOR_END: functional

expected = ["絵が多くて読みやすい", "ネタバレ（「第4章」を読んだら見えます）"]
assert lines_of(seen) == expected
assert lines == expected
try:
    line_of({"tag": "LostNote"})
    raise AssertionError("should refuse")
except ValueError:
    pass
print("ok")
