// Scouts sent out at the same time, for the chapter
// "Structured concurrency".
// 同時に送り出す偵察隊。「構造化された並行性」の章
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const report: string[] = [];
const sleep = (ms: number) =>
  new Promise<void>((resolve) => setTimeout(resolve, ms));

async function scout(name: string, ms: number): Promise<string> {
  await sleep(ms);
  report.push(name);
  return name;
}

async function trap(ms: number): Promise<string> {
  await sleep(ms);
  throw new Error("fell into a trap");
}

// ANCHOR: all
// TypeScript (Promise.all fails fast, but the others keep running)
// TypeScript（Promise.all はすぐ失敗を返すが、ほかは走り続ける）
async function party(): Promise<string[]> {
  return Promise.all([scout("slime", 50), trap(10), scout("goblin", 100)]);
}
// ANCHOR_END: all

async function main(): Promise<void> {
  let failed = false;
  try {
    await party();
  } catch {
    failed = true;
  }
  check(failed && report.length === 0, "fails fast");
  await sleep(200);
  check(report.join() === "slime,goblin", "the others kept running");
  console.log("ok");
}

main();
