#!/bin/sh
# Run the whole race of the benchmark chapter on this Mac, and print one
# table. Takes a few minutes. Run it on a quiet machine.
#   sh run-bench.sh          every library may use all the cores
#   sh run-bench.sh one      one thread for everybody (BLAS, PyTorch, JAX,
#                            massiv): the same work for every core count
# 速さを比べる章の比べっこを全部、この Mac で動かして、表にする（数分）。
# ほかに重い仕事をしていない機械で動かす。
#   sh run-bench.sh          どのライブラリも、全部のコアを使ってよい
#   sh run-bench.sh one      みんな1スレッド（BLAS・PyTorch・JAX・massiv）
set -eu
cd "$(dirname "$0")"
ROOT=../../..
PY=$ROOT/tools/python/.venv/bin/python
LANGS=$ROOT/code/langs/ml-benchmarks
export BOOK_BENCH=1
RTS="+RTS -N -RTS"
if [ "${1:-}" = one ]; then
  # Each library has its own knob for the number of threads.
  # スレッドの数のつまみは、ライブラリごとにちがう
  export VECLIB_MAXIMUM_THREADS=1 OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
  export XLA_FLAGS="--xla_cpu_multi_thread_eigen=false intra_op_parallelism_threads=1"
  RTS="+RTS -N1 -RTS"
  JULIA_THREADS=1
else
  JULIA_THREADS=auto
fi

cabal build -v0 exe:bench-ml exe:train-mnist
echo "== Haskell"
$(cabal list-bin bench-ml) $RTS
echo "== Python"
"$PY" "$LANGS/matmul.py"
"$PY" "$LANGS/softmax.py"
echo "== Julia"
julia --project=$ROOT/tools/julia --startup-file=no -t $JULIA_THREADS "$LANGS/matmul.jl"
julia --project=$ROOT/tools/julia --startup-file=no -t $JULIA_THREADS "$LANGS/softmax.jl"
echo "== R"
Rscript --vanilla "$LANGS/matmul.R"
Rscript --vanilla "$LANGS/softmax.R"
echo "== MNIST, one epoch / 1周"
printf 'MiniTorch (nn):    '; (cd ../nn && cabal build -v0 exe:train-mnist && $(cabal list-bin train-mnist) ../data/mnist | tail -1)
printf 'MiniTorch (bench): '; $(cabal list-bin train-mnist) ../data/mnist | tail -1
printf 'PyTorch:           '; "$PY" "$ROOT/code/langs/ml-neural-network/mnist.py" | tail -1
printf 'JAX:               '; "$PY" "$LANGS/mnist_jax.py" | tail -1
