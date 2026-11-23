// ANCHOR: all
// The tests of the answers of steps 1 and 2.
// ステップ1と2の解答のテスト
import { test } from "node:test";
import assert from "node:assert/strict";
import { makeRuntime } from "../../runtime/lam.mjs";
import { morePrims } from "./answers.mjs";

const rt = makeRuntime();
Object.assign(rt.prims, morePrims(rt));

// 「ゾーンの中の日付。UTC の 23:30 は、+09:00 では次の日」
test("Time.toDate gives the date in the zone", () => {
  const t = rt.time(BigInt(Date.UTC(2026, 10, 24, 23, 30)));
  const toDate = rt.prim("Time.toDate");
  assert.equal(toDate(rt.zone(540n))(t), "2026-11-25");
  assert.equal(toDate(rt.zone(0n))(t), "2026-11-24");
  assert.equal(toDate(rt.zone(-300n))(t), "2026-11-24");
});

// 「groupBy は Lam と同じに分け、10万個でもスタックを使い切らない」
test("groupBy groups as Lam does, even 100000 items", () => {
  const groupBy = rt.prim("Prelude.groupBy");
  const same = (a) => (b) => a === b;
  const xs = rt.list(1n, 1n, 2n, 1n);
  assert.equal(rt.show(groupBy(same)(xs)), "[[1, 1], [2], [1]]");
  const many = rt.fromArray(Array.from({ length: 100000 }, (_, i) =>
    BigInt(i)));
  assert.equal(rt.toArray(groupBy(same)(many)).length, 100000);
});
// ANCHOR_END: all
