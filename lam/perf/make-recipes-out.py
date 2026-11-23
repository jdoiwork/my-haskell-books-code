#!/usr/bin/env python3
# Make examples/recipes/<name>.out with the real lam: the transcript of
# <name>.cmd, as the book shows it. Each line of <name>.cmd that begins
# with "💻 $ " is a command (the lines after it that begin with spaces
# go on with it). The commands run one after another with sh, in a
# fresh copy of examples/recipes/, with lam on the PATH. <name>.setup
# (if there is one) runs there first, and is not shown. What a command
# writes (the standard output and the standard error, in the order it
# writes them) comes after the command; a command that ends with an exit
# code other than 0 shows "[exit N]". A command that ends with "&" (a
# server) runs in the background: what it writes first is shown once
# its port (187xx) is open. The processes the recipe left are stopped
# at the end.
# With --check, only compare: the real lam must give what the .out
# files say. Names after the flags pick some recipes only.
#
# 本物の lam で examples/recipes/<名前>.out を作る。<名前>.cmd の、本に
# 載せる形の記録。.cmd の「💻 $ 」で始まる行がコマンド（そのあとの空白で
# 始まる行は、その続き）。コマンドは、examples/recipes/ の新しい写しの
# 中で、PATH に lam を置いて、sh で順に動かす。<名前>.setup があれば先に
# 動かす（見せない）。コマンドが書いたもの（標準出力と標準エラーを、
# 書いた順に）をコマンドの下に置き、0 でない終了コードは「[exit N]」と
# 見せる。「&」で終わるコマンド（サーバー）は後ろで動かし、ポート（187xx）が
# 開いたら、それまでに書いたものを見せる。レシピが残したプロセスは最後に
# 止める。
# --check なら、くらべるだけ。フラグの後ろの名前で、レシピをしぼれる
import os, re, shlex, shutil, signal, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
recipes = os.path.join(here, "examples", "recipes")
prompt = "💻 $ "


def lam_path():
    out = subprocess.run(["cabal", "list-bin", "-v0", "exe:lam"], cwd=here,
                         capture_output=True, text=True, check=True)
    return out.stdout.strip()


def commands(text):
    """The commands of a .cmd file: each is a list of lines."""
    found = []
    for line in text.splitlines():
        if line.startswith(prompt):
            found.append([line])
        elif line.startswith(" ") and found:
            found[-1].append(line)
        elif line.strip() == "":
            continue
        else:
            raise SystemExit(f"a line that is not a command: {line!r}")
    return found


def background(i, body):
    """A server started with "&": keep what it writes in a file, wait
    until its port (187xx in the command) is open, then show it."""
    ports = re.findall(r"\b187\d\d\b", body)
    if not ports:
        raise SystemExit("a command with & and no port 187xx: " + body)
    log = f".background-{i}.txt"
    return "\n".join([
        "(\n" + body + "\n) > " + log + " 2>&1 < /dev/null &",
        "n=0",
        f"while ! nc -z 127.0.0.1 {ports[0]} 2>/dev/null; do",
        "  n=$((n + 1)); [ $n -gt 100 ] && break; sleep 0.1",
        "done",
        "sleep 0.2",
        "cat " + log])


def script(name, lines_list):
    """One sh script: the setup, then each command and its output."""
    parts = ["exec 2>&1"]
    setup = os.path.join(recipes, name + ".setup")
    if os.path.exists(setup):
        parts.append("sh ./" + name + ".setup > /dev/null 2>&1 || "
                     "{ echo 'the setup did not work'; exit 1; }")
    for i, lines in enumerate(lines_list):
        shown = "\n".join(lines)
        if i > 0:
            parts.append("echo")
        parts.append("printf '%s\\n' " + shlex.quote(shown))
        body = "\n".join([lines[0][len(prompt):]] + lines[1:])
        if body.rstrip().endswith("&"):
            parts.append(background(i, body.rstrip()[:-1]))
            continue
        # A pipe or a "<" inside the command still gives lam its input.
        # コマンドの中のパイプや「<」は、そのまま lam の入力になる
        parts.append("(\n" + body + "\n) < /dev/null")
        parts.append('code=$?; [ "$code" = 0 ] || echo "[exit $code]"')
    return "\n".join(parts) + "\n"


def run(name, lam):
    work = tempfile.mkdtemp(prefix="lam-recipes-")
    try:
        copy = os.path.join(work, "recipes")
        shutil.copytree(recipes, copy, ignore=shutil.ignore_patterns(
            "*.out", "*.cmd"))
        binary = os.path.join(work, "bin")
        os.mkdir(binary)
        os.symlink(lam, os.path.join(binary, "lam"))
        env = {
            "PATH": binary + os.pathsep + os.environ["PATH"],
            "HOME": work,
            "TZ": "Asia/Tokyo",
            "LANG": "en_US.UTF-8",
            "LC_ALL": "en_US.UTF-8",
            "LAM_SEED": "618",
            "GIT_CONFIG_GLOBAL": "/dev/null",
            "GIT_CONFIG_NOSYSTEM": "1",
        }
        text = open(os.path.join(recipes, name + ".cmd")).read()
        proc = subprocess.Popen(["sh", "-c", script(name, commands(text))],
                                cwd=copy, env=env, stdout=subprocess.PIPE,
                                start_new_session=True)
        try:
            out, _ = proc.communicate(timeout=120)
        finally:
            try:
                os.killpg(proc.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
        # The "\r" of HTTP's lines is dropped: the book shows lines.
        # HTTP の行の「\r」は落とす（本は行を見せる）
        text = out.decode("utf-8", "replace").replace("\r\n", "\n")
        return text.replace(work, "/tmp/w")
    finally:
        shutil.rmtree(work, ignore_errors=True)


def main():
    args = sys.argv[1:]
    check = "--check" in args
    names = [a for a in args if not a.startswith("--")]
    if not names:
        names = sorted(f[:-4] for f in os.listdir(recipes)
                       if f.endswith(".cmd"))
    lam = lam_path()
    status = 0
    for name in names:
        shown = run(name, lam)
        out = os.path.join(recipes, name + ".out")
        if check:
            want = open(out).read() if os.path.exists(out) else ""
            if want != shown:
                print("different: " + name)
                subprocess.run(["diff", out, "-"], input=shown.encode())
                status = 1
        else:
            with open(out, "w") as f:
                f.write(shown)
    sys.exit(status)


main()
