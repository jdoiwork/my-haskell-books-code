# Reading length-prefixed strings in Python, for the chapter
# "Protocol and parser".
# 「プロトコルとパーサー」の章の、長さが先に付いた文字列の読み方を Python で


# ANCHOR: procedural
# Python (procedural style: read the length first, wait if short)
# Python（手続き型スタイル：先に長さを読み、足りなければ待つ）
def parse_bulk(buffer):
    end = buffer.find(b"\r\n")
    if not buffer.startswith(b"$") or end < 0:
        return None                        # need more / 続きを待つ
    length = int(buffer[1:end])
    start = end + 2
    if len(buffer) < start + length + 2:
        return None                        # need more / 続きを待つ
    return buffer[start:start + length], buffer[start + length + 2:]
# ANCHOR_END: procedural

assert parse_bulk(b"$6\r\nLambda\r\n:42\r\n") == (b"Lambda", b":42\r\n")
assert parse_bulk(b"$6\r\nLam") is None

# ANCHOR: naive
# Python (splitting on lines breaks a value that contains \r\n)
# Python（行で区切ると、\r\n を含む値が壊れる）
message = b"SET note first-line\r\nsecond-line\r\n"
words = message.split(b"\r\n")[0].split()   # the value is cut in two
# ANCHOR_END: naive
assert words == [b"SET", b"note", b"first-line"]
assert parse_bulk(b"$23\r\nfirst-line\r\nsecond-line\r\n") == \
    (b"first-line\r\nsecond-line", b"")
print("ok")
