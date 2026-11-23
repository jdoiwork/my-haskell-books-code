// ANCHOR: all
// The answers of steps 1 and 2: more builtins for the runtime, in the
// same shape as its table (the name, how many arguments, what to do).
// Put them in with Object.assign(rt.prims, morePrims(rt)).
// ステップ1と2の解答：ランタイムに足す組み込み。表と同じ形（名前、
// 引数の数、すること）。Object.assign(rt.prims, morePrims(rt)) で入れる

// ANCHOR: toDate
// 📅 Step 1: Time.toDate zone t, the date of t in the zone:
//    "2026-11-25" for 23:30 in UTC, in +09:00.
// ステップ1：ゾーンの中の t の日付
function toDate(zone, t) {
  const minutes = Number(zone.args[0]);
  const local = new Date(Number(t.args[0]) + minutes * 60000);
  const two = (x) => String(x).padStart(2, "0");
  return String(local.getUTCFullYear()).padStart(4, "0") + "-" +
    two(local.getUTCMonth() + 1) + "-" + two(local.getUTCDate());
}
// ANCHOR_END: toDate

// ANCHOR: groupBy
// 🔁 Step 2: groupBy as a loop: the next item joins the group when
//    same says so with the group's first item (as Lam's does).
// ステップ2：ループで書いた groupBy。次の要素は、グループの最初の要素と
// same が言えば、そのグループに入る（Lam のものと同じ）
function groupBy(rt, same, xs) {
  const groups = [];
  for (const x of rt.toArray(xs)) {
    const last = groups[groups.length - 1];
    if (last && same(last[0])(x)) last.push(x);
    else groups.push([x]);
  }
  return rt.fromArray(groups.map((g) => rt.fromArray(g)));
}
// ANCHOR_END: groupBy

export function morePrims(rt) {
  return {
    "Time.toDate": [2, toDate],
    "Prelude.groupBy": [2, (same, xs) => groupBy(rt, same, xs)],
  };
}
// ANCHOR_END: all
