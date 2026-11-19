# The Python code in the chapter on scripts: sys.exit raises
# SystemExit, so finally (and with) runs on the way out; but
# os._exit ends at once, as Go's os.Exit does.
# スクリプトの章の Python のコード：sys.exit は SystemExit を投げるので、
# 出口で finally（と with）が動く。os._exit は、Go の os.Exit と同じく、
# その場で終わる
import os
import sys


# ANCHOR: python
# Python (sys.exit): finally runs on the way out
# Python（sys.exit）：出口で finally が動く
def main():
    print("the shop opens")
    try:
        sys.exit(3)
    finally:
        print("the shop closes")
# ANCHOR_END: python


# ANCHOR: pythonHard
# Python (os._exit): no finally, the same as Go's os.Exit
# Python（os._exit）：finally は動かない。Go の os.Exit と同じ
def main_hard():
    print("the shop opens", flush=True)
    try:
        os._exit(3)
    finally:
        print("the shop closes")
# ANCHOR_END: pythonHard


if __name__ == "__main__":
    main_hard() if sys.argv[1:] == ["hard"] else main()
