# Run the module programs of Python and JavaScript in run/, from their
# folders, and check what they print. cheat.mjs must be stopped.
# run/ の Python と JavaScript のモジュールのプログラムを、そのフォルダー
# で動かし、出したものを確かめる。cheat.mjs は止められなければならない
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
node = os.environ.get("BOOK_NODE", "node")

py = subprocess.run([sys.executable, "menu.py"], cwd=os.path.join(here, "run/py"),
                    capture_output=True, text=True, check=True).stdout
assert py == "1,200 yen\n350 yen\nnot a price\n-5 yen\n", py
print("ok menu.py")

js_dir = os.path.join(here, "run/js")
js = subprocess.run([node, "menu.mjs"], cwd=js_dir, capture_output=True,
                    text=True, check=True).stdout
assert js == "1,200 yen\n350 yen\nnot a price\nundefined\n", js
print("ok menu.mjs")

cheat = subprocess.run([node, "cheat.mjs"], cwd=js_dir, capture_output=True,
                       text=True)
assert cheat.returncode != 0, cheat
assert "does not provide an export named 'make'" in cheat.stderr, cheat.stderr
assert cheat.stdout == "", cheat.stdout
print("ok cheat.mjs (stopped)")
