# Play the Python arena with fixed input and check the messages.
# Python 版のアリーナを、決まった入力で遊んで、メッセージを確かめる
import os, subprocess, sys

arena = os.path.join(os.path.dirname(os.path.abspath(__file__)), "run", "arena.py")

def play(lines):
    return subprocess.run([sys.executable, arena], input="\n".join(lines) + "\n",
                          capture_output=True, text=True, check=True).stdout

out = play(["run"])
assert "Hero HP: 50" in out and "You ran away safely." in out, out
out = play(["dance", "r"])
assert "Huh? Try again." in out and "You ran away safely." in out, out
for _ in range(20):
    out = play(["h", "a"] * 30)
    assert ("You win!" in out) or ("Game over." in out), out
