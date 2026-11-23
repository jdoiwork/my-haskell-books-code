# Run the shell and Python one-liners in run/, each block in a fresh copy
# of the recipes of Lam (code/lam/perf/examples/recipes/, after the
# recipe's .setup), and compare with what Lam printed for that recipe
# (the output of the last command of <name>.out). The spaces in a line
# are not compared: uniq -c and Lam line up the numbers differently.
# run/ のシェルと Python のワンライナーを、ブロックごとに、Lam のレシピの
# フォルダーの新しい写し（レシピの .setup のあと）で動かし、Lam がその
# レシピで出したもの（<名前>.out の最後のコマンドの出力）とくらべる。行の
# 中の空白はくらべない（uniq -c と Lam で、数のそろえ方がちがう）
import os, re, shutil, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
recipes = os.path.join(here, "../../lam/perf/examples/recipes")


def blocks(path):
    """The ANCHOR blocks of a file: (name, the command)."""
    text = open(path).read()
    return re.findall(r"# ANCHOR: ([\w-]+)\n(.*?)# ANCHOR_END: \1", text, re.S)


def lam_answer(name):
    """What Lam printed for the last command of the recipe: the lines of
    <name>.out after the text of the last command of <name>.cmd."""
    out = open(os.path.join(recipes, name + ".out")).read()
    cmd = open(os.path.join(recipes, name + ".cmd")).read()
    last = "💻 $ " + cmd.split("💻 $ ")[-1].rstrip("\n") + "\n"
    return out[out.rindex(last) + len(last):].splitlines()


def same(lines):
    return [" ".join(l.split()) for l in lines]


env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1",
           LC_ALL="C")
for lang in ["sh", "python"]:
    for name, command in blocks(os.path.join(here, "run", lang + ".sh")):
        work = tempfile.mkdtemp()
        try:
            copy = os.path.join(work, "recipes")
            shutil.copytree(recipes, copy)
            setup = os.path.join(copy, name + ".setup")
            if os.path.exists(setup):
                subprocess.run(["sh", setup], cwd=copy, env=env,
                               capture_output=True, check=True)
            done = subprocess.run(["sh", "-c", command], cwd=copy, env=env,
                                  capture_output=True, text=True)
            assert done.returncode == 0, (lang, name, done.stderr)
            assert done.stderr == "", (lang, name, done.stderr)
            got = same(done.stdout.splitlines())
            want = same(lam_answer(name))
            assert got == want, (lang, name, got, want)
        finally:
            shutil.rmtree(work)
        print("ok", lang, name)
