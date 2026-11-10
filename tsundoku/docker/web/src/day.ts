// ANCHOR: all
// 📅 A day as the API reads it ("2026-11-10"), in the reader's own
//    time zone. toISOString() would give the day in UTC: in Japan,
//    before 9 in the morning, that is still yesterday.
// API が読む形の日付（"2026-11-10"）。読む人の時間帯で。
// toISOString() だと UTC の日付になる。日本では、朝9時より前は、
// まだ昨日になってしまう
export function dayOf(at: Date): string {
  const two = (n: number) => String(n).padStart(2, "0");
  return `${at.getFullYear()}-${two(at.getMonth() + 1)}-${two(at.getDate())}`;
}
// ANCHOR_END: all
