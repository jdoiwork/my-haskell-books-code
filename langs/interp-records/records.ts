// The TypeScript code in the chapter on type annotations and records.
// 型の注釈とレコードの章の TypeScript のコード

// ANCHOR: structural
// TypeScript (structural types)
// TypeScript（構造的な型）
type Member = { name: string; lang: string; days: number };

// Anything with a name: the other fields may be there, too.
// name を持つものなら何でも。ほかのフィールドがあってもよい
function label(r: { name: string }): string {
  return "[" + r.name + "]";
}

// A new object with days changed: the spread copies the rest.
// days だけ変えた新しいオブジェクト。スプレッドが残りを写す
function nextDay(m: Member): Member {
  return { ...m, days: m.days + 1 };
}
// ANCHOR_END: structural

// ANCHOR: excess
// TypeScript (an object literal is checked for extra fields)
// TypeScript（オブジェクトのリテラルは、余計なフィールドを調べられる）
const ruby = { name: "Ruby", version: "4.0" };
label(ruby);                                  // OK: through a variable
// @ts-expect-error: 'version' does not exist in type '{ name: string; }'
label({ name: "Ruby", version: "4.0" });      // an error, written here
// ANCHOR_END: excess

const lam: Member = { name: "Lam", lang: "lam", days: 19 };
console.assert(label(nextDay(lam)) === "[Lam]");
console.assert(nextDay(lam).days === 20 && lam.days === 19);
console.assert(label(ruby) === "[Ruby]");
console.log(label(nextDay(lam)), label(ruby));
