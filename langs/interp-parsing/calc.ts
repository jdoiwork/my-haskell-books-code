// The calculator of interp-parsing.md, with typed parser combinators.
// 「文字列を読み解く」の章の電卓を、型の付いたパーサーの組み合わせで

// ANCHOR: types
// TypeScript (functional style): Parser<T> reads from pos, and gives
// a T and the next pos, or null.
// TypeScript（関数型スタイル）：Parser<T> は pos から読み、T と次の
// pos か、null をくれる
type Parser<T> = (text: string, pos: number) => [T, number] | null;

const token = (s: string): Parser<string> => (text, pos) => {
  while (text[pos] === " ") pos++;
  return text.startsWith(s, pos) ? [s, pos + s.length] : null;
};

const number: Parser<number> = (text, pos) => {
  while (text[pos] === " ") pos++;
  const m = /^\d+/.exec(text.slice(pos));
  return m ? [Number(m[0]), pos + m[0].length] : null;
};

// The first parser that fits.
// 最初に合ったパーサー
const oneOf = <T>(...ps: Parser<T>[]): Parser<T> => (text, pos) => {
  for (const p of ps) {
    const found = p(text, pos);
    if (found) return found;
  }
  return null;
};

// Read with p, then with the parser that f makes: Parser.andThen.
// p で読み、f が作るパーサーで続きを読む（Parser.andThen）
const andThen = <A, B>(p: Parser<A>, f: (a: A) => Parser<B>): Parser<B> =>
  (text, pos) => {
    const found = p(text, pos);
    return found ? f(found[0])(text, found[1]) : null;
  };

const pure = <T>(x: T): Parser<T> => (_text, pos) => [x, pos];

// Make the parser only when it reads: Parser.lazy.
// 読むときになってから、パーサーを作る（Parser.lazy）
const lazy = <T>(f: () => Parser<T>): Parser<T> => (text, pos) =>
  f()(text, pos);
// ANCHOR_END: types

// ANCHOR: calc
type Op = (a: number, b: number) => number;
const ops: Record<string, Op> = {
  "+": (a, b) => a + b,
  "-": (a, b) => a - b,
  "*": (a, b) => a * b,
  "/": (a, b) => Math.floor(a / b),
};

// item (operator item)*, joined from the left.
// item のあとに「演算子 item」が続く。左からまとめる
const chain = (item: Parser<number>, op: Parser<string>): Parser<number> =>
  andThen(item, (first) => (text, pos) => {
    let value = first;
    for (;;) {
      const next = andThen(op, (o) =>
        andThen(item, (x) => pure(ops[o](value, x))))(text, pos);
      if (!next) return [value, pos];
      [value, pos] = next;
    }
  });

// p between two others.
// 2つのものではさまれた p
const between = <T>(
  open: Parser<string>, p: Parser<T>, close: Parser<string>,
): Parser<T> =>
  andThen(open, () => andThen(p, (x) => andThen(close, () => pure(x))));

// expr is used before it is made, so it is wrapped in lazy.
// expr は、できる前に使うので、lazy で包む
const factor = oneOf(
  number,
  between(token("("), lazy(() => expr), token(")")),
);
const term = chain(factor, oneOf(token("*"), token("/")));
const expr: Parser<number> = chain(term, oneOf(token("+"), token("-")));
// ANCHOR_END: calc

const calc = (text: string): number | null => {
  const found = expr(text, 0);
  return found && text.slice(found[1]).trim() === "" ? found[0] : null;
};

const cases: [string, number][] = [
  ["1 + 2 * 3", 7],
  ["(1 + 2) * 3", 9],
  [" (1 + (2 * 3)) * 4 ", 28],
  ["10 - 4 - 3", 3],
];
for (const [text, value] of cases) {
  if (calc(text) !== value) throw new Error(text);
}
if (calc("(1 + 2") !== null) throw new Error("unclosed");
console.log("ok");
