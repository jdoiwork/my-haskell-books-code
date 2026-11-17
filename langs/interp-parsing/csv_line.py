# A line of CSV with quotes, as in interp-parsing.md.
# 「文字列を読み解く」の章の、引用符つきの CSV の1行
import csv


# ANCHOR: procedural
# Python (procedural style): one letter at a time, remembering whether
# we are inside quotes
# Python（手続き型スタイル）：1文字ずつ、引用符の中にいるかを覚えておく
def fields_of(line):
    fields = []
    field = ""
    in_quotes = False
    i = 0
    while i < len(line):
        c = line[i]
        if in_quotes:
            if c == '"' and line[i + 1:i + 2] == '"':
                field += '"'        # "" is one quote / "" は引用符1つ
                i += 1
            elif c == '"':
                in_quotes = False
            else:
                field += c
        elif c == '"':
            in_quotes = True
        elif c == ",":
            fields.append(field)
            field = ""
        else:
            field += c
        i += 1
    if in_quotes:
        raise ValueError("a closing quote is missing")
    fields.append(field)
    return fields
# ANCHOR_END: procedural


# ANCHOR: library
# Python (the standard library): csv.reader knows the quotes
# Python（標準ライブラリ）：csv.reader は引用符を知っている
def fields_of_csv(line):
    return next(csv.reader([line], strict=True))
# ANCHOR_END: library


lines = {
    '"Lam T-shirt, large",2500,3': ["Lam T-shirt, large", "2500", "3"],
    "mug,1200,": ["mug", "1200", ""],
    '"say ""hi""",0,1': ['say "hi"', "0", "1"],
}
for line, fields in lines.items():
    assert fields_of(line) == fields, line
    assert fields_of_csv(line) == fields, line

# A comma in quotes cuts the field, when we only split.
# split だけでは、引用符の中のカンマでも切れる
assert '"Lam T-shirt, large",2500'.split(",") == ['"Lam T-shirt', ' large"', "2500"]

# ANCHOR: errors
for f in (fields_of, fields_of_csv):
    try:
        f('"open,1')
        assert False
    except (ValueError, csv.Error) as e:
        print(type(e).__name__, e)
# ANCHOR_END: errors
