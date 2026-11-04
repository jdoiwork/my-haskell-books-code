// The TypeScript code in the type checker chapter.
// 型検査器の章の TypeScript のコード

// ANCHOR: noinfer
// TypeScript (the types of parameters are not guessed from the body)
// TypeScript（引数の型は、本体からは推論されない）
// @ts-expect-error: Parameter 'f' implicitly has an 'any' type.
const twiceUntyped = (f, x) => f(f(x));

// With a type variable A written by hand, it works for any A.
// 型変数 A を手で書けば、どんな A でも使える
const twice = <A>(f: (a: A) => A, x: A): A => f(f(x));

const n: number = twice((k: number) => k * 2, 5);       // 20
const s: string = twice((t: string) => t + "!", "hi");  // "hi!!"
// ANCHOR_END: noinfer

void twiceUntyped;
if (n !== 20 || s !== "hi!!") {
  throw new Error("twice failed");
}
console.log("ok");
