// The zod code in the error handling chapter.
// エラー処理の章の zod のコード
// ANCHOR: zod
// TypeScript (zod)
import { z } from "zod";

// Turn the input into a number, then check it is an integer of 15 or more.
// 入力を数に変換（coerce）してから、整数（int）で15以上（min(15)）かを調べる
const Age = z.coerce.number().int().min(15);

const result = Age.safeParse("13");
if (result.success) {
  console.log(result.data + 1); // result.data is a number / 数になっている
} else {
  console.log(result.error.issues[0].message);
}
// ANCHOR_END: zod
if (result.success || Age.safeParse("16").data !== 16) throw new Error("failed: zod");
