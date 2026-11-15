# The autodiff chapter in Julia: ForwardDiff carries slopes forward with
# dual numbers, like Dual.hs.
# 自動微分の章を Julia で：ForwardDiff は、Dual.hs と同じく、二重数で
# 傾きを前から運ぶ
# ANCHOR: forwarddiff
# Julia: f has no types written, so it works for any kind of number
# Julia：f には型を書いていないので、どんな種類の数でも使える
using ForwardDiff

f(x, y) = x * y + sin(x)

println(ForwardDiff.derivative(x -> f(x, 3), 2.0))
println(ForwardDiff.gradient(v -> f(v[1], v[2]), [2.0, 3.0]))
# The number that goes through f: a dual number
# f の中を通る数：二重数
println(typeof(ForwardDiff.Dual(2.0, 1.0)))
# ANCHOR_END: forwarddiff
@assert ForwardDiff.derivative(x -> f(x, 3), 2.0) == 3 + cos(2.0)
@assert ForwardDiff.gradient(v -> f(v[1], v[2]), [2.0, 3.0]) ≈ [3 + cos(2.0), 2.0]
println("ok")
