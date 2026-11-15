# Softmax of each row of a 1000 × 1000 matrix in Julia: loops, and dots
# (broadcasting).
# 1000 × 1000 の行列の、行ごとの softmax を Julia で：ループと、ドット（ブロードキャスト）
using BenchmarkTools
using Printf
using Random

const FULL = get(ENV, "BOOK_BENCH", "") == "1"
report(task, n, how, ms) = @printf("%-8s n=%-5d %-14s %10.2f ms\n", task, n, how, ms)
median_ms(trial) = median(trial).time / 1e6

# ANCHOR: loops
# Julia (procedural style): one row at a time, into one new matrix
# Julia（手続き型スタイル）：1行ずつ、新しい行列1つに書いていく
function softmax_rows_loops(x)
    out = similar(x)
    for i in axes(x, 1)
        m = maximum(@view x[i, :])
        s = 0.0
        for j in axes(x, 2)
            out[i, j] = exp(x[i, j] - m)
            s += out[i, j]
        end
        for j in axes(x, 2)
            out[i, j] /= s
        end
    end
    return out
end
# ANCHOR_END: loops

# ANCHOR: dots
# Julia (dots): like NumPy, but the dots of one line fuse into one loop
# Julia（ドット）：NumPy に似ているが、1行のドットは1つのループに融合する
function softmax_rows_dots(x)
    e = exp.(x .- maximum(x, dims=2))
    return e ./ sum(e, dims=2)
end
# ANCHOR_END: dots

let x = rand(MersenneTwister(3), 5, 7)
    @assert softmax_rows_loops(x) ≈ softmax_rows_dots(x)
    @assert all(sum(softmax_rows_dots(x), dims=2) .≈ 1)
end

n = FULL ? 1000 : 50
x = rand(MersenneTwister(3), n, n)
report("softmax", n, "Julia loops", median_ms(@benchmark sum(softmax_rows_loops($x)) samples=20 evals=1))
report("softmax", n, "Julia dots", median_ms(@benchmark sum(softmax_rows_dots($x)) samples=20 evals=1))
