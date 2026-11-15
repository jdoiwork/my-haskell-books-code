# The NumPy chapter: how R lays out a matrix, and how it stretches vectors.
# NumPy 風の配列の章：R の行列の並べ方と、ベクトルの引き伸ばし方

# ANCHOR: layout
# R: 1 to 6 fill the matrix column by column; counting starts at 1
# R：1 から 6 を、列から順に埋める。数えるのは 1 から
m <- matrix(1:6, nrow = 2, ncol = 3)
print(m)
print(m[1, 2])
# ANCHOR_END: layout
stopifnot(identical(as.vector(m), 1:6))
stopifnot(m[1, 2] == 3, m[2, 1] == 2)

# ANCHOR: recycle
# R: a shorter vector is used again and again, in the order the numbers
# are laid out (column by column), so c(10, 20) goes down each column
# R：短いベクトルを、数の並びの順（列から）にくり返して使う。
# だから c(10, 20) は、どの列にも上から足される
print(m + c(10, 20))
# A length that does not divide 6 is only a warning
# 6 を割り切れない長さでも、警告だけで計算する
print(m + c(10, 20, 30, 40))
# ANCHOR_END: recycle
stopifnot(identical(m + c(10, 20), matrix(c(11, 22, 13, 24, 15, 26), 2, 3)))
warned <- tryCatch({ m + c(10, 20, 30, 40); FALSE }, warning = function(w) TRUE)
stopifnot(warned)
cat("ok\n")
