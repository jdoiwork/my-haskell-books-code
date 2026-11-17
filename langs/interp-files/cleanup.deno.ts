// Remove the old .log files, in Deno (TypeScript). check-langs runs it
// with no flags: Deno stops each door, and says which flag to add.
// 古い .log ファイルを消す。Deno で。check-langs はフラグなしで動かす：
// Deno は、どの扉も止めて、足すフラグを言う
// ANCHOR: cleanup
// deno run --allow-read=logs --allow-write=logs cleanup.deno.ts logs
const DAY = 24 * 60 * 60 * 1000;

// 🧹 Remove the .log files that are some days old or more.
// 何日か以上たった .log ファイルを消す
async function cleanup(folder: string, days = 7): Promise<string[]> {
  const removed: string[] = [];
  for await (const entry of Deno.readDir(folder)) {
    if (!entry.isFile || !entry.name.endsWith(".log")) continue;
    const path = `${folder}/${entry.name}`;
    const info = await Deno.stat(path);
    const age = Math.floor((Date.now() - info.mtime!.getTime()) / DAY);
    if (age >= days) {
      await Deno.remove(path);
      removed.push(path);
    }
  }
  return removed;
}

// 👥 git log, with no shell: the command and a list of arguments.
// git log をシェルなしで：コマンドと引数のリスト
async function authors(): Promise<string[]> {
  const command = new Deno.Command("git", {
    args: ["log", "--format=%an"],
  });
  const { code, stdout } = await command.output();
  if (code !== 0) throw new Error(`git log ended with ${code}`);
  return new TextDecoder().decode(stdout).split("\n").filter((l) => l);
}
// ANCHOR_END: cleanup

// Stop with an error when a check fails (console.assert only prints).
// 確かめが外れたら、エラーで止まる（console.assert は書くだけ）
function check(ok: boolean, ...what: unknown[]): void {
  if (!ok) throw new Error(`check failed: ${JSON.stringify(what)}`);
}

async function needs(what: () => Promise<unknown>): Promise<string> {
  try {
    await what();
    return "ok";
  } catch (e) {
    return `${(e as Error).name}: ${(e as Error).message}`;
  }
}

// ANCHOR: denied
const read = await needs(() => cleanup("logs"));
const run = await needs(() => authors());
// ANCHOR_END: denied
console.log(read);
console.log(run);
check(read === 'NotCapable: Requires read access to "logs", ' +
  "run again with the --allow-read flag", read);
check(run === 'NotCapable: Requires run access to "git", ' +
  "run again with the --allow-run flag", run);
