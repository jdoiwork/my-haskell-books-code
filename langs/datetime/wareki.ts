// The TypeScript code in the "dates and times" chapter: Japanese eras with Intl.
// 日付と時刻の章の TypeScript のコード：Intl で和暦
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: wareki
// TypeScript (Intl reads the era table of ICU, which comes from CLDR)
// TypeScript（Intl は ICU の元号の表を読む。表のもとは CLDR）
const wareki = new Intl.DateTimeFormat("ja-JP-u-ca-japanese", {
  era: "long", year: "numeric", month: "long", day: "numeric",
  timeZone: "UTC",
});
const before = wareki.format(new Date("1868-10-22"));  // depends on ICU
const first = wareki.format(new Date("1868-10-23"));   // "明治元年10月23日"
const reiwa = wareki.format(new Date("2019-05-01"));   // "令和元年5月1日"
// ANCHOR_END: wareki

check(first === "明治元年10月23日", first);
check(reiwa === "令和元年5月1日", reiwa);
// Before Meiji: "慶応4年…" with CLDR 48, "西暦1868年…" with CLDR 49.
// 明治より前は、CLDR 48 なら「慶応4年…」、CLDR 49 なら「西暦1868年…」
check(!before.startsWith("明治"), before);
console.log("ok");
