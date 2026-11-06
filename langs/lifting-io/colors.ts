// The TypeScript code in the "lifting IO" chapter.
// IO を持ち上げるの章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

const said: string[] = [];
function shout(message: string): void {
  said.push(`[board] ${message}`);
}

// ANCHOR: inside
// TypeScript (inside async: calling a plain function is easy)
// TypeScript（async の中から、ふつうの関数を呼ぶのは簡単）
async function appraise(item: string): Promise<number> {
  await sleep(100);
  return item.length * 100;
}

async function appraiseAll(items: string[]): Promise<number[]> {
  shout("appraising..."); // a plain function / ふつうの関数
  return Promise.all(items.map(appraise));
}
// ANCHOR_END: inside

// ANCHOR: outside
// TypeScript (a plain function cannot wait: the answer comes later)
// TypeScript（ふつうの関数は待てない。答えは、あとで届く）
function closingReport(items: string[], done: (total: number) => void) {
  // const prices = await appraiseAll(items);  // error outside async
  appraiseAll(items).then((prices) => {
    done(prices.reduce((a, b) => a + b, 0));
  });
}
// ANCHOR_END: outside

// ANCHOR: color
// TypeScript (so the caller becomes async too, all the way up)
// TypeScript（だから呼ぶ側も async に。上へ上へと広がる）
async function closingReportAsync(items: string[]): Promise<number> {
  const prices = await appraiseAll(items);
  return prices.reduce((a, b) => a + b, 0);
}
// ANCHOR_END: color

const start = Date.now();
const total = await new Promise<number>((resolve) =>
  closingReport(["sword", "shield", "herb"], resolve));
check(total === 1500, "callback total");
check(Date.now() - start < 250, "at the same time");
check(await closingReportAsync(["herb"]) === 400, "async total");
check(said.length === 2, "shouted twice");
console.log("ok");
