// The TypeScript code in the chapter on data and case.
// data と case の章の TypeScript のコード

// ANCHOR: union
// TypeScript: a union of objects, told apart by kind
// TypeScript：kind で見分ける、オブジェクトの共用体
type Shape =
  | { kind: "circle"; r: number }
  | { kind: "rect"; w: number; h: number };

function area(s: Shape): number {
  switch (s.kind) {
    case "circle":
      return 3 * s.r * s.r;
    case "rect":
      return s.w * s.h;
  }
}

console.log(area({ kind: "rect", w: 3, h: 4 }));   // 12
// ANCHOR_END: union

if (area({ kind: "circle", r: 2 }) !== 12) throw new Error("circle");
if (area({ kind: "rect", w: 3, h: 4 }) !== 12) throw new Error("rect");
