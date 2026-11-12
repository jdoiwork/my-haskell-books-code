// The TypeScript code in the "dates and times" chapter.
// 日付と時刻の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

function same<T>(actual: T, expected: T): void {
  check(JSON.stringify(actual) === JSON.stringify(expected),
        `${JSON.stringify(actual)} = ${JSON.stringify(expected)}`);
}

// ANCHOR: date
// TypeScript (procedural style: with Date, a length is just milliseconds)
// TypeScript（手続き型スタイル：Date では、長さはただのミリ秒）
const posted = new Date("2026-11-13T09:00:00Z");
const limit = new Date(posted.getTime() + 72 * 60 * 60 * 1000);
let payDay = new Date("2027-01-31T00:00:00Z");
payDay.setUTCMonth(payDay.getUTCMonth() + 1);  // rolls over to 03-03
// ANCHOR_END: date

same(limit.toISOString(), "2026-11-16T09:00:00.000Z");
same(payDay.toISOString().slice(0, 10), "2027-03-03");

// ANCHOR: duration
// TypeScript (functional style: Temporal has a Duration type)
// TypeScript（関数型スタイル：Temporal には Duration の型がある）
const rush = Temporal.Duration.from({ hours: 72 });
const left = Temporal.Duration.from({ hours: 26, minutes: 5, seconds: 7 });
const term = Temporal.Duration.from({ months: 1, days: 10 });
const ends = Temporal.PlainDate.from("2027-01-31").add(term);
const served = Temporal.PlainDate.from("2027-01-31")
  .until("2027-03-15", { largestUnit: "months" });
// ANCHOR_END: duration

same(rush.total("hours"), 72);
same(left.toString(), "PT26H5M7S");
same(term.toString(), "P1M10D");
same(ends.toString(), "2027-03-10");
same(served.toString(), "P1M15D");
const firstPay = Temporal.PlainDate.from("2027-01-31");
same([0, 1, 2, 3].map((n) => firstPay.add({ months: n }).toString()),
     ["2027-01-31", "2027-02-28", "2027-03-31", "2027-04-30"]);
console.log("ok");
