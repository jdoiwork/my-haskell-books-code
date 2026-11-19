// The TypeScript code in the chapter on classes and instances.
// 型クラスを作るの章の TypeScript のコード

type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rect"; width: number; height: number };

type Yen = { yen: number };

// ANCHOR: procedural
// TypeScript (procedural style)
// TypeScript（手続き型スタイル）
function describeShape(s: Shape): string {
  switch (s.kind) {
    case "circle":
      return `a circle of radius ${s.radius}`;
    case "rect":
      return `a ${s.width}x${s.height} rect`;
  }
}

function describeYen(m: Yen): string {
  return `${m.yen} yen`;
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style: a dictionary by hand)
// TypeScript（関数型スタイル：手で書く辞書）
interface Describe<A> {
  describe: (x: A) => string;
}

const shapeDescribe: Describe<Shape> = { describe: describeShape };
const yenDescribe: Describe<Yen> = { describe: describeYen };

const twice = <A>(d: Describe<A>, x: A): string =>
  d.describe(x) + ", " + d.describe(x);

// A dictionary made from a dictionary: Describe a => Describe (Maybe a)
// 辞書から作る辞書
const maybeDescribe = <A>(d: Describe<A>): Describe<A | null> => ({
  describe: (m) => (m === null ? "nothing" : "just " + d.describe(m)),
});
// ANCHOR_END: functional

// ANCHOR: empty
// TypeScript (empty needs no argument: it is in the dictionary)
// TypeScript（empty には引数が要らない。辞書の中にある）
interface Combine<A> {
  empty: A;
  combine: (a: A, b: A) => A;
}

const yenCombine: Combine<Yen> = {
  empty: { yen: 0 },
  combine: (a, b) => ({ yen: a.yen + b.yen }),
};

const total = <A>(d: Combine<A>, xs: A[]): A =>
  xs.reduce(d.combine, d.empty);
// ANCHOR_END: empty

const check = (ok: boolean, what: string): void => {
  if (!ok) throw new Error(what);
};
check(twice(shapeDescribe, { kind: "circle", radius: 2 })
        === "a circle of radius 2, a circle of radius 2", "twice shape");
check(twice(yenDescribe, { yen: 80 }) === "80 yen, 80 yen", "twice yen");
check(maybeDescribe(shapeDescribe).describe(null) === "nothing", "nothing");
check(maybeDescribe(maybeDescribe(yenDescribe)).describe({ yen: 3 })
        === "just just 3 yen", "maybe maybe");
check(total(yenCombine, [{ yen: 300 }, { yen: 450 }]).yen === 750, "total");
check(total(yenCombine, []).yen === 0, "empty");
console.log("ok");
