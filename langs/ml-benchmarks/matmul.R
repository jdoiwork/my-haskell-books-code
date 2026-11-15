# The matrix product of two n × n matrices in R: three loops, and %*% (BLAS).
# n × n の行列の積を R で：3重ループと、%*%（BLAS）
full <- Sys.getenv("BOOK_BENCH") == "1"

# The median of reps runs in ms, after one warm-up run.
# 1回温めてから reps 回動かした中央値（ミリ秒）
median_ms <- function(reps, f) {
  f()
  times <- replicate(reps, {
    t0 <- Sys.time()
    f()
    as.numeric(Sys.time() - t0, units = "secs")
  })
  median(times) * 1000
}
report <- function(task, n, how, ms) {
  cat(sprintf("%-8s n=%-5d %-14s %10.2f ms\n", task, n, how, ms))
}

# ANCHOR: naive
# R (procedural style): three loops
# R（手続き型スタイル）：3重ループ
naive_matmul <- function(a, b) {
  n <- nrow(a); k <- ncol(a); m <- ncol(b)
  c <- matrix(0, n, m)
  for (i in 1:n) {
    for (j in 1:m) {
      s <- 0
      for (t in 1:k) {
        s <- s + a[i, t] * b[t, j]
      }
      c[i, j] <- s
    }
  }
  c
}
# ANCHOR_END: naive

set.seed(1)
a <- matrix(runif(400), 20, 20); b <- matrix(runif(400), 20, 20)
stopifnot(isTRUE(all.equal(naive_matmul(a, b), a %*% b)))

for (n in if (full) c(256, 512, 1024) else c(32)) {
  a <- matrix(runif(n * n), n, n)
  b <- matrix(runif(n * n), n, n)
  if (n <= 256) report("matmul", n, "R naive", median_ms(3, function() sum(naive_matmul(a, b))))
  report("matmul", n, "R %*%", median_ms(10, function() sum(a %*% b)))
}
