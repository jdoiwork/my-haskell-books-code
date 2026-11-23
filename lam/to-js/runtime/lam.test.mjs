// ANCHOR: all
// The tests of the runtime, with node's own test runner:
//   node --test runtime/
// ランタイムのテスト。node の付属のテストの道具で
import { test } from "node:test";
import assert from "node:assert/strict";
import { makeRuntime } from "./lam.mjs";

// ANCHOR: fake
// 🎭 A fake world: it writes to a list instead of the screen. The same
//    idea as lam's fake world: the runtime does not know the difference.
// 偽物の世界：画面ではなく、リストに書く。lam の偽物の世界と同じ考え。
// ランタイムには、ちがいがわからない
function fakeWorld(screen) {
  return async (request, job, rt) => {
    if (request.tag === "PutStr") {
      screen.push(request.text);
      return rt.unit;
    }
    if (request.tag === "Sleep") return rt.unit;
    throw new Error("the fake world cannot do " + request.tag);
  };
}
// ANCHOR_END: fake

const rt = makeRuntime();
const call = (name, ...args) =>
  args.reduce((f, x) => f(x), rt.prim(name));

// ANCHOR: numbers
// 「// は下へ丸める。BigInt の / とはちがう」
test("// rounds down, as Haskell's div", () => {
  assert.equal(rt.intDiv(-7n, 2n), -4n);
  assert.equal(rt.intDiv(7n, -2n), -4n);
  assert.equal(rt.intDiv(7n, 2n), 3n);
  assert.equal(-7n / 2n, -3n);
  assert.throws(() => rt.intDiv(1n, 0n), /divided by zero/);
});

// 「Int と Float が混ざると、Float の計算」
test("an Int next to a Float is a Float", () => {
  assert.equal(rt.add(1n, 2n), 3n);
  assert.equal(rt.add(1.5, 1n), 2.5);
  assert.equal(rt.eq(NaN, NaN), false);
  assert.equal(rt.show(call("round", -2.5)), "Just (-3)");
  assert.equal(call("Float.fixed", 2n, 1.005), "1.00");
});
// ANCHOR_END: numbers

// ANCHOR: strings
// 「文字はコードポイントで数え、コードポイントの順で比べる」
test("letters are code points", () => {
  assert.equal(call("length", "🧾日本"), 3n);
  assert.equal("🧾日本".length, 4);
  assert.equal(call("String.take", 1n, "🧾日本"), "🧾");
  assert.equal(rt.lt("｡", "😀"), true);
  assert.equal("｡" < "😀", false);
  assert.equal(call("padLeft", 4n, "🧾"), "   🧾");
});
// ANCHOR_END: strings

// ANCHOR: showJson
// 「lam と同じ形で見せる。JSON の数は正確なまま」
test("shows values as lam does", () => {
  const xs = rt.list(rt.just(-3n), rt.nothing);
  assert.equal(rt.show(xs), "[Just (-3), Nothing]");
  assert.equal(rt.show({ name: "Lam", age: 3n }),
               "{ age = 3, name = \"Lam\" }");
  const j = call("Json.parse", "[12345678901234567890, 19.99, 1e-7]");
  assert.equal(call("Json.show", j.args[0]),
               "[12345678901234567890,19.99,1.0e-7]");
});
// ANCHOR_END: showJson

// ANCHOR: runFake
// 「頼みごとは、偽物の世界でも動く。百万回の bind でも、スタックは平ら」
test("runs requests in a fake world, a million binds deep", async () => {
  const screen = [];
  const fake = makeRuntime(fakeWorld(screen));
  const put = fake.prim("IO.putStrLn");
  const bind = fake.prim("IO.bindIO");
  const pure = fake.prim("IO.returnIO");
  let io = pure(0n);
  for (let i = 0; i < 1000000; i++) io = bind(io)((n) => pure(n + 1n));
  const done = await fake.run(bind(io)((n) => put(String(n))),
                              { cancelled: false });
  assert.deepEqual(screen, ["1000000\n"]);
  assert.ok("value" in done);
});
// ANCHOR_END: runFake
// ANCHOR_END: all
