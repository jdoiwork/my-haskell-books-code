# The Python code in the evaluator chapter.
# 評価器の章の Python のコード

# ANCHOR: eval
# Python (an evaluator: the environment is a dict, a closure is a tuple)
# Python（評価器：環境は dict、クロージャはタプル）
def evaluate(env, e):
    kind = e[0]
    if kind == "lit":
        return e[1]
    if kind == "var":
        return env[e[1]]
    if kind == "lam":                       # ("lam", x, body)
        return ("closure", e[1], e[2], env)
    if kind == "add":
        return evaluate(env, e[1]) + evaluate(env, e[2])
    if kind == "app":                       # ("app", f, a)
        _, x, body, closed = evaluate(env, e[1])
        arg = evaluate(env, e[2])
        return evaluate({**closed, x: arg}, body)
    raise ValueError(kind)
# ANCHOR_END: eval

add = ("lam", "x", ("lam", "y", ("add", ("var", "x"), ("var", "y"))))
inc = ("app", add, ("lit", 1))
assert evaluate({}, ("app", inc, ("lit", 41))) == 42

# ANCHOR: late
# Python (a closure sees the variable, not the value at that time)
# Python（クロージャが見るのは、そのときの値ではなく、変数そのもの）
fs = []
for i in range(3):
    fs.append(lambda: i)
print([f() for f in fs])     # [2, 2, 2]

gs = [lambda i=i: i for i in range(3)]
print([g() for g in gs])     # [0, 1, 2]
# ANCHOR_END: late

assert [f() for f in fs] == [2, 2, 2]
assert [g() for g in gs] == [0, 1, 2]
