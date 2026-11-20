# TypeScript (tsc 7) stops on a regex literal with a wrong pattern, at
# compile time. The file is made here, so that check-langs.sh does not
# take it for a program to run.
# TypeScript（tsc 7）は、まちがったパターンの正規表現リテラルで、
# コンパイルのときに止まる。ファイルはここで作る（check-langs.sh が、
# 動かすプログラムとまちがえないように）
import os
import subprocess
import tempfile

here = os.path.dirname(os.path.abspath(__file__))
tsc = os.path.join(here, "../../../tools/typescript/node_modules/.bin/tsc")

# ANCHOR: literal
broken = 'const date = /(?<year>\\d{4})-(?<month>\\d{2}/;\n'
# ANCHOR_END: literal

with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "broken.ts")
    with open(path, "w") as f:
        f.write(broken)
    p = subprocess.run([tsc, "--strict", "--noEmit", "--target", "es2025",
                        "broken.ts"], cwd=d, capture_output=True, text=True)
    assert p.returncode != 0
    assert p.stdout.strip() == "broken.ts(1,44): error TS1005: ')' expected.", p.stdout
    print(p.stdout.strip())
