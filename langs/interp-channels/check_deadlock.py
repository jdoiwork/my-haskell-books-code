# Run the Go program that forgets to close its channel, and check that Go
# finds the deadlock. / チャネルを閉じ忘れる Go のプログラムを動かし、Go が
# デッドロックを見つけることを確かめる
import os, subprocess

here = os.path.dirname(os.path.abspath(__file__))
go = os.environ.get("BOOK_GO", "go")
r = subprocess.run([go, "run", os.path.join(here, "run", "deadlock.go")],
                   capture_output=True, text=True)
assert r.returncode != 0, r
assert r.stdout == "got 1\ngot 2\ngot 3\n", r.stdout
assert "fatal error: all goroutines are asleep - deadlock!" in r.stderr, r.stderr
print(r.stderr.splitlines()[0])
