# 🧾 Where is the + on this line? Three ways to count: the letters
#    (code points), the units of UTF-16 (LSP), and the bytes (UTF-8).
# この行の + は、どこ？ 3つの数え方：文字（コードポイント）、UTF-16 の
# 単位（LSP）、バイト（UTF-8）
line = 'f"🧾 total: {total items + 100}"'
before = line[: line.index("+")]


# ANCHOR: procedural
# Python (procedural style)
# Python（手続き型スタイル）
def utf16_units(text):
    units = 0
    for ch in text:
        if ord(ch) > 0xFFFF:
            units += 2
        else:
            units += 1
    return units
# ANCHOR_END: procedural


# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
def utf16_units_fn(text):
    return sum(2 if ord(ch) > 0xFFFF else 1 for ch in text)
# ANCHOR_END: functional


# ANCHOR: lengths
letters = len(before)                         # 24
utf16 = len(before.encode("utf-16-le")) // 2  # 25
utf8 = len(before.encode("utf-8"))            # 27
# ANCHOR_END: lengths

assert (letters, utf16, utf8) == (24, 25, 27)
assert utf16_units(before) == utf16_units_fn(before) == 25
print("columns.py: ok")
