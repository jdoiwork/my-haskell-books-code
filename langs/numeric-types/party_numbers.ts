// The numbers of the chapter "Number types and conversions", in TypeScript.
// 「数の型と変換」の章の数を、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: average
// TypeScript
const levels: number[] = [12, 15, 9, 20];
const total = levels.reduce((sum, level) => sum + level, 0);
// number is one type for both 56 and 14.5
// number は、56 も 14.5 も同じ1つの型
const average = total / levels.length;   // 14
const damage = Math.floor(33 * 1.5);     // 49
// ANCHOR_END: average
check(average === 14, "average");
check(damage === 49, "damage");

// ANCHOR: narrow
// TypeScript
// an 8-bit array wraps, like Int8
// 8 ビットの配列は、Int8 と同じく一周する
const wrapped = new Int8Array([300])[0]; // 44
// bigint is another type: convert it by hand
// bigint は別の型。手で変換する
const exp = 100n * 2n ** BigInt(60);     // 115292150460684697600n
// ANCHOR_END: narrow
check(wrapped === 44, "wrapped");
check(exp === 115292150460684697600n, "exp");
console.log("ok");
