# The matrix product of two n × n matrices in Julia: three loops, and
# `*` (BLAS). BOOK_BENCH=1 measures for the book; otherwise small sizes,
# only to check the answers.
# n × n の行列の積を Julia で：3重ループと、`*`（BLAS）。
# BOOK_BENCH=1 で本のために測る。それ以外は小さくして、答えを確かめるだけ
using BenchmarkTools
using LinearAlgebra
using Printf
using Random

const FULL = get(ENV, "BOOK_BENCH", "") == "1"

# ANCHOR: naive
# Julia (procedural style): three loops, compiled to machine code
# Julia（手続き型スタイル）：3重ループ。機械語にコンパイルされる
function naive_matmul(a, b)
    n, k = size(a)
    m = size(b, 2)
    c = zeros(n, m)
    for j in 1:m, i in 1:n          # i inside: Julia keeps columns together
        s = 0.0
        for t in 1:k
            s += a[i, t] * b[t, j]
        end
        c[i, j] = s
    end
    return c
end
# ANCHOR_END: naive

report(task, n, how, ms) =
    @printf("%-8s n=%-5d %-14s %10.2f ms\n", task, n, how, ms)

# ANCHOR: bench
# The median of many runs, in ms. `$a` hands the value in, so that
# BenchmarkTools does not time looking up a global variable.
# 何回も動かした中央値（ミリ秒）。`$a` で値を渡すと、
# グローバル変数を探す時間まで測らずに済む
median_ms(trial) = median(trial).time / 1e6
# ANCHOR_END: bench

# Check: the loops and BLAS give the same answer.
# 確かめ：ループと BLAS は同じ答え
let a = rand(MersenneTwister(1), 20, 20), b = rand(MersenneTwister(2), 20, 20)
    @assert naive_matmul(a, b) ≈ a * b
end

for n in (FULL ? (256, 512, 1024) : (32,))
    a = rand(MersenneTwister(1), n, n)
    b = rand(MersenneTwister(2), n, n)
    if n <= 512
        report("matmul", n, "Julia naive",
               median_ms(@benchmark sum(naive_matmul($a, $b)) samples=5 evals=1))
    end
    report("matmul", n, "Julia BLAS",
           median_ms(@benchmark sum($a * $b) samples=20 evals=1))
end
