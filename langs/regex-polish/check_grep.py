# Run the cases of minigrep (code/miniregex/polish/testdata/grep/cases.sh)
# with the grep of macOS (-E), GNU grep 3.12 (-P) and ripgrep 15.2.0, and
# check that each prints the same lines as minigrep (expected.out). GNU
# grep and ripgrep run in Docker; the image is made from grep.Dockerfile
# when it is missing. Without Docker, they are skipped with a message.
# minigrep のためし（code/miniregex/polish/testdata/grep/cases.sh）を、
# macOS の grep（-E）・GNU grep 3.12（-P）・ripgrep 15.2.0 で動かし、どれも
# minigrep と同じ行を出す（expected.out）のを確かめる。GNU grep と
# ripgrep は Docker の中で。イメージがなければ grep.Dockerfile から作る。
# Docker がなければ、知らせて飛ばす
import hashlib
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
cases = os.path.normpath(
    os.path.join(here, "../../miniregex/polish/testdata/grep"))
want = open(os.path.join(cases, "expected.out"), encoding="utf-8").read()


def check(name, out):
    assert out == want, f"{name}:\n{out}"
    print(f"{name}: the same as minigrep")


# The grep of macOS knows \d and \w with -E. Its \w is Unicode only in a
# UTF-8 locale (with LANG unset, ラムダ is not a word).
# macOS の grep は、-E で \d と \w がわかる。\w が Unicode になるのは、
# UTF-8 のロケールのときだけ（LANG がないと、ラムダは単語ではない）
if os.path.exists("/usr/bin/grep") and os.uname().sysname == "Darwin":
    env = dict(os.environ, GREP="/usr/bin/grep -E", LC_ALL="en_US.UTF-8")
    check("grep -E (macOS)",
          subprocess.run(["sh", "cases.sh"], cwd=cases, env=env,
                         capture_output=True, text=True).stdout)
    env["LC_ALL"] = "C"
    out = subprocess.run(["sh", "cases.sh"], cwd=cases, env=env,
                         capture_output=True, text=True).stdout
    assert "\nラムダ sold\n" not in out and "\nLam sold\n" in out, out
    print("grep -E (macOS, LC_ALL=C): \\w is ASCII only")

dockerfile = os.path.join(here, "grep.Dockerfile")
tag = hashlib.md5(open(dockerfile, "rb").read()).hexdigest()[:12]
image = f"book-grep:{tag}"

if subprocess.run(["docker", "info"], capture_output=True).returncode != 0:
    print("skip: no Docker for GNU grep and ripgrep")
else:
    if subprocess.run(["docker", "image", "inspect", image],
                      capture_output=True).returncode != 0:
        subprocess.run(["docker", "build", "-q", "-t", image, "-f", dockerfile,
                        here], check=True, capture_output=True)

    def in_docker(grep):
        return subprocess.run(
            ["docker", "run", "--rm", "--network", "none",
             "-v", f"{cases}:/work:ro", "-e", f"GREP={grep}", image,
             "sh", "cases.sh"],
            capture_output=True, text=True, check=True).stdout

    check("GNU grep -P", in_docker("grep -P"))
    # ripgrep reads files in parallel: --sort=path keeps their order
    # ripgrep はファイルを並行に読む。--sort=path で順番をそろえる
    check("ripgrep", in_docker("rg --sort=path"))
    versions = subprocess.run(
        ["docker", "run", "--rm", image, "sh", "-c",
         "grep -V | head -1; rg --version | head -1"],
        capture_output=True, text=True, check=True).stdout
    assert "GNU grep) 3.12" in versions and "ripgrep 15.2.0" in versions
