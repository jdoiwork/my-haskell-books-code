#!/bin/sh
# Run both, one thread each. / どちらも1スレッドで動かす
set -eu
export OMP_NUM_THREADS=1
hasktorch-matmul
/opt/py/bin/python bench/matmul.py
