# The scikit-learn chapter in Julia: the same medicine prices, and X \ y.
# scikit-learn の章を Julia で：同じ薬の値段と、X \ y
sizes = [i / 5 for i in 0:49]
prices = [3 * sizes[i + 1] + 5 + sin(1.7 * i) for i in 0:49]

# ANCHOR: backslash
# Julia: put a column of ones beside sizes (for the intercept), and
# solve "X times [w, b] is as close to prices as it can be"
# Julia：sizes の横に 1 の列を並べ（切片のため）、
# 「X に [w, b] を掛けたものが、prices にいちばん近い」を解く
X = [sizes ones(50)]
w, b = X \ prices
println((w, b))
# ANCHOR_END: backslash
@assert abs(w - 3.00116745397) < 1e-9 && abs(b - 5.01347253972) < 1e-9
println("ok")
