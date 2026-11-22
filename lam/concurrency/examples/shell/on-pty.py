# (on-pty.py: not tty.py, the name of a module of Python.)
# Run lam on a pseudo terminal (a pty), as if a person watched it, and
# print what it drew. Term.isTerminal says true there.
# lam を擬似端末（pty）の上で動かし、描いたものを出す。そこでは
# Term.isTerminal が true になる
#   python3 on-pty.py <lam> <args...>
import os
import pty
import subprocess
import sys

main, slave = pty.openpty()
proc = subprocess.Popen(sys.argv[1:], stdin=slave, stdout=slave,
                        stderr=slave, close_fds=True)
os.close(slave)
shown = b""
while True:
    try:
        piece = os.read(main, 1024)
    except OSError:
        break
    if not piece:
        break
    shown += piece
proc.wait()
# A terminal ends each line with \r\n: keep only \n.
# 端末は行を \r\n で終える。\n だけにする
sys.stdout.write(shown.decode().replace("\r\n", "\n"))
