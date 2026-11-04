// The TypeScript code in the concurrency chapter.
// 並行・並列の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: all
// TypeScript (Promise.all: wait for many at once)
// TypeScript（Promise.all：いくつもを同時に待つ）
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

async function scout(place: string, millis: number): Promise<string> {
  await sleep(millis);
  return `${place}: ${Math.floor(millis / 100)} goblins`;
}

async function scoutsAtOnce(places: [string, number][]): Promise<string[]> {
  return Promise.all(places.map(([p, m]) => scout(p, m)));
}
// ANCHOR_END: all

const start = Date.now();
const reports = await scoutsAtOnce([["forest", 300], ["cave", 200],
                                    ["tower", 100]]);
check(Date.now() - start < 500, "time");
check(reports.join() === "forest: 3 goblins,cave: 2 goblins,tower: 1 goblins",
      "reports");
