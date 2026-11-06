// The TypeScript code in the "choosing a box" chapter.
// 書き換えられる箱の使い分けの章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

// ANCHOR: counter
// TypeScript (a closure holds the box: a let only take() can reach)
// TypeScript（クロージャーが箱を持つ：take() からしか届かない let）
function makeTicketBox(): () => number {
  let next = 1;
  return () => next++;
}
// ANCHOR_END: counter

const take = makeTicketBox();
check([take(), take(), take()].join() === "1,2,3", "tickets");

let calls = 0;
async function slowAppraise(item: string): Promise<number> {
  calls++;
  await sleep(100);
  return item.length * 100;
}

// ANCHOR: once
// TypeScript (keep the Promise itself: those who come later wait for it)
// TypeScript（Promise そのものを覚える：後から来た人は、それを待つ）
const prices = new Map<string, Promise<number>>();

function appraiseOnce(item: string): Promise<number> {
  let price = prices.get(item);
  if (price === undefined) {
    price = slowAppraise(item);
    // A failure is not remembered. / 失敗は覚えない
    price.catch(() => prices.delete(item));
    prices.set(item, price);
  }
  return price;
}
// ANCHOR_END: once

const start = Date.now();
const items = [...Array(5).fill("sword"), ...Array(5).fill("shield")];
const answers = await Promise.all(items.map(appraiseOnce));
check(calls === 2, "two appraisals");
check(Date.now() - start < 180, "in parallel");
check(answers.join() === "500,500,500,500,500,600,600,600,600,600",
      "answers");
console.log("ok");
