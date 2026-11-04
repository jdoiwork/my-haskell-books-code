// Does Promise keep the Monad laws? For the chapter
// "Functor, Applicative and Monad, deeper".
// Promise は Monad の法則を守る？
// 「Functor・Applicative・Monad をもっと深く」の章
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: thenable
// TypeScript (left identity: wrap x, then hand it to f ... or call f(x))
// TypeScript（左単位元：x を包んでから f に渡す……と、f(x) は同じ？）
const x = { then: (resolve: (v: number) => void) => resolve(42) };
const f = (v: unknown) => Promise.resolve(typeof v);

const wrapped = Promise.resolve(x).then(f); // "number"
const direct = f(x);                        // "object"
// ANCHOR_END: thenable

Promise.all([wrapped, direct]).then(([a, b]) => {
  check(a === "number", "Promise.resolve looked inside the thenable");
  check(b === "object", "f got the object itself");
  console.log("ok");
});
