# Softmax of each row of a 1000 × 1000 matrix in R: whole-matrix steps.
# 1000 × 1000 の行列の、行ごとの softmax を R で：行列まるごとの計算で
full <- Sys.getenv("BOOK_BENCH") == "1"
median_ms <- function(reps, f) {
  f()
  times <- replicate(reps, { t0 <- Sys.time(); f(); as.numeric(Sys.time() - t0, units = "secs") })
  median(times) * 1000
}
report <- function(task, n, how, ms) cat(sprintf("%-8s n=%-5d %-14s %10.2f ms\n", task, n, how, ms))

# ANCHOR: vector
# R (vectors): every step works on the whole matrix, like NumPy
# R（ベクトル）：NumPy と同じく、どの1歩も行列まるごとに
softmax_rows <- function(x) {
  e <- exp(x - apply(x, 1, max))
  e / rowSums(e)
}
# ANCHOR_END: vector

set.seed(3)
x <- matrix(runif(35), 5, 7)
p <- softmax_rows(x)
stopifnot(isTRUE(all.equal(rowSums(p), rep(1, 5))))
stopifnot(isTRUE(all.equal(p[2, ], exp(x[2, ]) / sum(exp(x[2, ])))))

n <- if (full) 1000 else 50
x <- matrix(runif(n * n), n, n)
report("softmax", n, "R", median_ms(10, function() sum(softmax_rows(x))))
