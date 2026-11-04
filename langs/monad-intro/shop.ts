// The TypeScript code in the Monad intro chapter.
// Monad 入門の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const prices = new Map([["potion", 50], ["elixir", 150], ["sword", 300]]);

// ANCHOR: promise
// TypeScript (a Promise: then is like (>>=), await is like do)
// TypeScript（Promise：then は (>>=)、await は do に近い）
async function buy(item: string, gold: number): Promise<number> {
  const p = prices.get(item);
  if (p === undefined) throw new Error(`${item} is not for sale`);
  if (p > gold) throw new Error(`need ${p - gold} more gold`);
  return gold - p;
}

// with then: each step hands its result to the next
// then で：1歩ずつ、結果を次へ渡す
function buyThreeThen(a: string, b: string, c: string,
                      gold: number): Promise<number> {
  return buy(a, gold).then((g1) => buy(b, g1)).then((g2) => buy(c, g2));
}

// with await: the same chain, written step by step
// await で：同じつながりを、1行ずつ
async function buyThree(a: string, b: string, c: string,
                        gold: number): Promise<number> {
  const g1 = await buy(a, gold);
  const g2 = await buy(b, g1);
  return buy(c, g2);
}
// ANCHOR_END: promise

check(await buyThree("potion", "elixir", "potion", 300) === 50, "ok");
check(await buyThreeThen("potion", "elixir", "potion", 300) === 50, "then");
const failure = await buyThree("potion", "sword", "potion", 300)
  .then(() => "no error", (e: Error) => e.message);
check(failure === "need 50 more gold", "failure");
