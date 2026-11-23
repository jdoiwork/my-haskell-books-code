# Check run/slow.awk: three lines, two of them slow failures of one IP.
# run/slow.awk を確かめる：3行のうち2行が、同じ IP の遅い失敗
import os
import subprocess
import tempfile

rows = [
    '10.0.1.2 - - [x] "GET / HTTP/1.1" 503 10 rt=0.900',
    '10.0.1.2 - - [x] "GET / HTTP/1.1" 500 10 rt=0.600',
    '10.0.1.3 - - [x] "GET / HTTP/1.1" 200 10 rt=0.900',
    '10.0.1.4 - - [x] "GET / HTTP/1.1" 500 10 rt=0.400',
]
here = os.path.dirname(os.path.abspath(__file__))
with tempfile.NamedTemporaryFile("w", delete=False) as f:
    f.write("\n".join(rows) + "\n")
try:
    out = subprocess.run(
        ["awk", "-f", os.path.join(here, "run", "slow.awk"), f.name],
        capture_output=True, text=True, check=True).stdout
    assert out == "    2  10.0.1.2\n", out
finally:
    os.remove(f.name)
