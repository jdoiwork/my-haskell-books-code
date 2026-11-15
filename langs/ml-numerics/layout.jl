# The NumPy chapter: how Julia lays out a matrix, and its dots.
# NumPy 風の配列の章：Julia の行列の並べ方と、ドット

# ANCHOR: layout
# Julia: the numbers are kept column by column; counting starts at 1
# Julia：数は列から順に並ぶ。数えるのは 1 から
A = [1 2 3; 4 5 6]
println(vec(A))      # the numbers in memory order / メモリーの上の順
println(A[1, 2])
# ANCHOR_END: layout
@assert vec(A) == [1, 4, 2, 5, 3, 6]
@assert A[1, 2] == 2

# ANCHOR: dots
# Julia: element-wise needs a dot. Shapes line up from the left, and a
# vector is a column, so [10, 20] goes to the rows
# Julia：要素ごとの計算には、ドットを付ける。形は左からそろえ、
# ベクトルは列なので、[10, 20] は行ごとに足される
println(A .+ [10, 20])
println(A .+ [10 20 30])      # a 1 × 3 row / 1 行 3 列
try
    A .+ [10, 20, 30]
catch e
    println(sprint(showerror, e))
end
# ANCHOR_END: dots
@assert A .+ [10, 20] == [11 12 13; 24 25 26]
@assert A .+ [10 20 30] == [11 22 33; 14 25 36]
@assert try A .+ [10, 20, 30]; false catch e e isa DimensionMismatch end
println("ok")
