// TypeScript (Promise): many waits at once, the ways to take a
// failure, and a race. "fetchMon" only waits, so no network is needed.
// TypeScript（Promise）：たくさんの待ちを同時に、失敗の受け止め方、競走。
// fetchMon は待つだけなので、ネットは要らない
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

const delays: Record<string, number> = {
  bulbasaur: 300, charmander: 500, squirtle: 200,
  pikachu: 400, eevee: 300, snorlax: 600,
};
const finished: string[] = [];

async function fetchMon(name: string): Promise<string> {
  await sleep(delays[name] ?? 100);
  if (!(name in delays)) throw new Error(`${name}: 404`);
  finished.push(name);
  return name.toUpperCase();
}

// ANCHOR: procedural
// TypeScript (procedural style): await one by one
// TypeScript（手続き型スタイル）：1つずつ await する
async function oneByOne(names: string[]): Promise<string[]> {
  const result: string[] = [];
  for (const name of names) {
    result.push(await fetchMon(name));
  }
  return result;
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style): map starts them all, Promise.all waits
// TypeScript（関数型スタイル）：map で全部を始め、Promise.all で待つ
const allAtOnce = (names: string[]) => Promise.all(names.map(fetchMon));
// ANCHOR_END: functional

// ANCHOR: failures
// IO.settle: allSettled gives every answer, fulfilled or rejected
// IO.settle：allSettled は、うまくいったものも失敗も、全部の答えをくれる
async function settle(names: string[]) {
  const results = await Promise.allSettled(names.map(fetchMon));
  return {
    oks: results.flatMap((r) => (r.status === "fulfilled" ? [r.value] : [])),
    errs: results.flatMap((r) =>
      r.status === "rejected" ? [String((r.reason as Error).message)] : []),
  };
}
// ANCHOR_END: failures

async function timed<T>(p: () => Promise<T>): Promise<[T, number]> {
  const start = Date.now();
  const value = await p();
  return [value, (Date.now() - start) / 1000];
}

const names = Object.keys(delays);
const [slow, t1] = await timed(() => oneByOne(names));
const [fast, t2] = await timed(() => allAtOnce(names));
check(slow.join() === fast.join(), "the same answers");
check(t1 > 2.25 && t1 < 2.6, `one by one: ${t1}`);
check(t2 > 0.55 && t2 < 0.8, `all at once: ${t2}`);

const piles = await settle(["pikachu", "nosuchmon", "eevee"]);
check(piles.oks.join() === "PIKACHU,EEVEE", "oks");
check(piles.errs.join() === "nosuchmon: 404", "errs");

// ANCHOR: race
// IO.race: the first to settle wins. Promise.any: the first fulfilled.
// IO.race：最初に終わったものが勝ち。Promise.any：最初にうまくいったもの
const winner =
  await Promise.race([fetchMon("snorlax"), fetchMon("squirtle")]);
const firstOk = await Promise.any([fetchMon("nosuchmon"), fetchMon("eevee")]);
// ANCHOR_END: race
check(winner === "SQUIRTLE", "race");
check(firstOk === "EEVEE", "any");
console.log("ok");
