# The Python code in the polish chapter.
# 仕上げの章の Python のコード

# ANCHOR: church
# Python (Church numerals with lambda)
# Python（lambda で作るチャーチ数）
zero = lambda f: lambda x: x
succ = lambda n: lambda f: lambda x: f(n(f)(x))
plus = lambda m: lambda n: lambda f: lambda x: m(f)(n(f)(x))
times = lambda m: lambda n: lambda f: m(n(f))
to_int = lambda n: n(lambda k: k + 1)(0)

two = succ(succ(zero))
three = succ(two)
print(to_int(times(plus(two)(three))(three)))   # 15
# ANCHOR_END: church

assert to_int(times(plus(two)(three))(three)) == 15
