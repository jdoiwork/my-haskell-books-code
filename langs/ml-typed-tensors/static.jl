# The typed-tensors chapter in Julia: StaticArrays puts the sizes in the
# type, but a wrong product stops only when it runs.
# 型で次元を守る章を Julia で：StaticArrays は大きさを型に持つが、
# 合わない積が止まるのは、動かしたとき
using StaticArrays

# ANCHOR: static
# Julia (StaticArrays): the sizes are part of the type
# Julia（StaticArrays）：大きさが型の一部になる
a = @SMatrix ones(2, 3)
b = @SMatrix ones(3, 4)
println(typeof(a * b))

# Defining a function with a wrong product is fine...
# 合わない積を書いた関数も、定義はできる……
wrong(m) = m * @SMatrix ones(4, 4)
println("defined wrong")
# ...it stops when it is called
# ……止まるのは、呼んだとき
try
    wrong(a)
catch e
    println(sprint(showerror, e))
end
# ANCHOR_END: static
@assert a * b isa SMatrix{2, 4}
@assert try wrong(a); false catch e e isa DimensionMismatch end
println("ok")
