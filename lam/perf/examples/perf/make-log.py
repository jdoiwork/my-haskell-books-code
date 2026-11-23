# 📜 Make a log of nginx-like lines (200,000 by default; the seed is 618).
#    python3 make-log.py [LINES] [FILE]
# nginx 風のログを作る（既定は 20 万行。乱数の種は 618）
import random
import sys

random.seed(618)
paths = ["/", "/login", "/api/todos", "/api/todos/12", "/books",
         "/static/app.js", "/search?q=haskell"]
n = int(sys.argv[1]) if len(sys.argv) > 1 else 200000
out = sys.argv[2] if len(sys.argv) > 2 else "big.log"
with open(out, "w") as f:
    for i in range(n):
        ip = f"10.0.{random.randint(0, 9)}.{random.randint(1, 40)}"
        st = random.choices([200, 302, 404, 500, 503], [80, 8, 6, 4, 2])[0]
        ms = random.randint(1, 1500)
        when = f"20/Nov/2026:10:{i // 3600 % 60:02d}:{i % 60:02d} +0900"
        path = random.choice(paths)
        size = random.randint(100, 9000)
        f.write(f'{ip} - - [{when}] "GET {path} HTTP/1.1" '
                f'{st} {size} rt={ms / 1000:.3f}\n')
