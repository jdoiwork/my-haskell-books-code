# ANCHOR: all
# The tests of the runtime, with Python's own unittest:
#   python3 -I -m unittest discover -s runtime
# ランタイムのテスト。Python の付属の unittest で
import asyncio
import math
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lam  # noqa: E402  (after the path is set / 道を入れてから)


# ANCHOR: fake
# 🎭 A fake world: it writes to a list instead of the screen. The same
#    idea as lam's fake world: the runtime does not know the difference.
# 偽物の世界：画面ではなく、リストに書く。lam の偽物の世界と同じ考え。
# ランタイムには、ちがいがわからない
def fake_world(screen):
    async def world(request, job, rt):
        if request["tag"] == "PutStr":
            screen.append(request["text"])
            return rt.unit
        raise Exception("the fake world cannot do " + request["tag"])
    return world
# ANCHOR_END: fake


def call(name, *args):
    f = lam.prim(name)
    for x in args:
        f = f(x)
    return f


class Numbers(unittest.TestCase):
    # ANCHOR: numbers
    # 「int と // は、そのまま Lam と同じ。/ と math は、例外の代わりに値を」
    def test_ints_and_floats(self):
        self.assertEqual(-7 // 2, -4)
        self.assertEqual(2 ** 70 + 1, 1180591620717411303425)
        self.assertEqual(lam.div(1, 0), math.inf)
        self.assertTrue(math.isnan(call("sqrt", -1)))
        self.assertEqual(call("Float.log", 0), -math.inf)
        with self.assertRaises(ZeroDivisionError):
            1 / 0
        self.assertEqual(lam.message_of(ZeroDivisionError()),
                         "divided by zero")

    # 「Float は、Lam（JavaScript）の形で見せる。repr とはちがう」
    def test_show_float(self):
        cases = [(1.0, "1"), (0.1 + 0.2, "0.30000000000000004"),
                 (1e16, "10000000000000000"), (1e21, "1e+21"),
                 (1e-7, "1e-7"), (-2.5, "-2.5"), (123.456, "123.456")]
        for x, shown in cases:
            self.assertEqual(lam.show_float(x), shown)
        self.assertEqual(repr(1e-7), "1e-07")
        self.assertEqual(lam.show(call("round", -2.5)), "Just (-3)")
        self.assertEqual(round(2.5), 2)
        self.assertEqual(call("Float.fixed", 2, 0.125), "0.13")
        self.assertEqual(format(0.125, ".2f"), "0.12")
    # ANCHOR_END: numbers


class Strings(unittest.TestCase):
    # ANCHOR: strings
    # 「Python の str はコードポイントの並び。手助けなしで Lam と同じ」
    def test_code_points(self):
        self.assertEqual(call("length", "🧾日本"), 3)
        self.assertEqual(len("🧾日本"), 3)
        self.assertEqual(call("String.take", 1, "🧾日本"), "🧾")
        self.assertTrue(lam.lt("｡", "😀"))
        self.assertTrue("｡" < "😀")
        self.assertEqual(call("padLeft", 4, "🧾"), "   🧾")
    # ANCHOR_END: strings


class Show(unittest.TestCase):
    # ANCHOR: showJson
    # 「lam と同じ形で見せる。JSON の数は正確なまま」
    def test_show(self):
        xs = lam.lam_list(lam.just(-3), lam.nothing)
        self.assertEqual(lam.show(xs), "[Just (-3), Nothing]")
        self.assertEqual(lam.show({"name": "Lam", "age": 3}),
                         '{ age = 3, name = "Lam" }')
        self.assertEqual(lam.show((True, ())), "(true, ())")
        j = call("Json.parse", "[12345678901234567890, 19.99, 1e-7]")
        self.assertEqual(call("Json.show", j.args[0]),
                         "[12345678901234567890,19.99,1.0e-7]")
    # ANCHOR_END: showJson


class Run(unittest.TestCase):
    # ANCHOR: runFake
    # 「頼みごとは、偽物の世界でも動く。百万回の bind でも、深い呼び出しに
    # ならない」
    def test_a_million_binds(self):
        screen = []
        rt = lam.make_runtime(fake_world(screen))
        put = rt.prim("IO.putStrLn")
        bind = rt.prim("IO.bindIO")
        pure = rt.prim("IO.returnIO")

        def loop(n):
            if n == 0:
                return put("done")
            return bind(pure(n))(lambda _: loop(n - 1))
        done = asyncio.run(rt.run(loop(1_000_000), lam.Job()))
        self.assertEqual(done, {"value": ()})
        self.assertEqual(screen, ["done\n"])
    # ANCHOR_END: runFake


class Closures(unittest.TestCase):
    # ANCHOR: closures
    # 「ループの中の lambda は、変数をつかまえる。既定の引数なら、値を」
    def test_late_binding(self):
        late = []
        now = []
        for n in range(1, 4):
            late.append(lambda x: x * n)
            now.append(lambda x, n=n: x * n)
        self.assertEqual([f(10) for f in late], [30, 30, 30])
        self.assertEqual([f(10) for f in now], [10, 20, 30])
    # ANCHOR_END: closures


if __name__ == "__main__":
    unittest.main()
# ANCHOR_END: all
