// The TypeScript code in the "time zones and summer time" chapter.
// タイムゾーンと夏時間の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

function same<T>(actual: T, expected: T): void {
  check(JSON.stringify(actual) === JSON.stringify(expected),
        `${JSON.stringify(actual)} = ${JSON.stringify(expected)}`);
}

// ANCHOR: date
// TypeScript (procedural style: one Date holds only milliseconds)
// TypeScript（手続き型スタイル：Date が持つのはミリ秒だけ）
const posted = new Date("2026-11-12T21:00:00Z");
const millis = posted.getTime();             // 1794517200000
const utcDay = posted.toISOString().slice(0, 10);       // "2026-11-12"
const tokyoDay = posted.toLocaleDateString("sv-SE", {
  timeZone: "Asia/Tokyo",
});                                          // "2026-11-13"
// ANCHOR_END: date

same(millis, 1794517200000);
same(utcDay, "2026-11-12");
same(tokyoDay, "2026-11-13");

// ANCHOR: parse
// TypeScript (the same-looking strings are read differently)
// TypeScript（似た文字なのに、読み方がちがう）
const dateOnly = new Date("2026-11-13");       // UTC midnight
const dateTime = new Date("2026-11-13T00:00"); // local midnight
// ANCHOR_END: parse

same(dateOnly.toISOString(), "2026-11-13T00:00:00.000Z");
const offsetMinutes = new Date("2026-11-13T00:00").getTimezoneOffset();
same(dateTime.getTime() - dateOnly.getTime(), offsetMinutes * 60000);

// ANCHOR: intl
// TypeScript (functional style: every branch's wall clock with Intl)
// TypeScript（関数型スタイル：Intl で全部の支部の壁の時計を）
const branches = ["Asia/Tokyo", "America/New_York", "Europe/London"];
const walls = branches.map((timeZone) =>
  new Intl.DateTimeFormat("en-GB", {
    timeZone,
    hour: "2-digit",
    minute: "2-digit",
    timeZoneName: "short",
  }).format(posted),
);
// ANCHOR_END: intl

same(walls, [
  "06:00 GMT+9",
  "16:00 GMT-5",
  "21:00 GMT",
]);

// ANCHOR: temporal
// TypeScript (Temporal: separate types, like Haskell's time)
// TypeScript（Temporal：Haskell の time のように、型を分ける）
const moment = Temporal.Instant.from("2026-11-12T21:00:00Z");
const wall = moment.toZonedDateTimeISO("Asia/Tokyo");
const today = wall.toPlainDate();                       // a day only
const due = today.add({ days: 7 });                     // "2026-11-20"
// ANCHOR_END: temporal

same(wall.toString(), "2026-11-13T06:00:00+09:00[Asia/Tokyo]");
same(today.toString(), "2026-11-13");
same(due.toString(), "2026-11-20");
console.log("ok");
