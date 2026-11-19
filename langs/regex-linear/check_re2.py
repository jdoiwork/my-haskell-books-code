# Build re2.cc with RE2 (C++) in Docker, run it, and check what it
# prints. The image is made from re2.Dockerfile when it is missing.
# Without Docker, it is skipped with a message.
# re2.cc を Docker の中の RE2（C++）でコンパイルして動かし、出力を
# 確かめる。イメージがなければ re2.Dockerfile から作る。Docker が
# なければ、知らせて飛ばす
import hashlib
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
dockerfile = os.path.join(here, "re2.Dockerfile")
tag = hashlib.md5(open(dockerfile, "rb").read()).hexdigest()[:12]
image = f"book-re2:{tag}"
want = "0\n0 invalid escape sequence: \\1\na\nab\n"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for RE2")
else:
    if subprocess.run(["docker", "image", "inspect", image],
                      capture_output=True).returncode != 0:
        subprocess.run(["docker", "build", "-q", "-t", image, "-f", dockerfile,
                        here], check=True, capture_output=True)
    out = subprocess.run(
        ["docker", "run", "--rm", "--network", "none",
         "-v", f"{here}:/src:ro", image, "sh", "-c",
         "g++ -std=c++17 -O2 /src/re2.cc $(pkg-config --cflags --libs re2)"
         " -o re2demo && ./re2demo"],
        capture_output=True, text=True, check=True).stdout
    assert out == want, out
    print(out, end="")
