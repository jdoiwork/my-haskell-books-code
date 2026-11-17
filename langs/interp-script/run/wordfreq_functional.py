#!/usr/bin/env python3
# ANCHOR: functional
# Python (functional style)
# Python（関数型スタイル）
import fileinput
import re
import sys
from collections import Counter

# fileinput reads the files in sys.argv, or the standard input.
# fileinput は、sys.argv のファイルか、標準入力を読む
try:
    text = "".join(fileinput.input(encoding="utf-8"))
except OSError as e:
    sys.exit(f"wordfreq: {e.filename}: {e.strerror}")

counts = Counter(re.findall(r"[^\W\d_]+", text.lower()))
ranking = sorted(counts.items(), key=lambda pair: (-pair[1], pair[0]))
print("\n".join(f"{w:<12}{n}" for w, n in ranking[:10]))
# ANCHOR_END: functional
