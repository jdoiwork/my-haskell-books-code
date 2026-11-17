// The Pokédex cache of the chapter, in Deno (TypeScript). check-langs
// runs it with no flags, in Docker without a network: the permission
// errors are checked first, and then the cache with a fake web.
// 章のポケモン図鑑のキャッシュを Deno で。check-langs は、フラグなし・
// ネットなしの Docker で動かす。先に権限のエラーを、次に偽物の Web で
// キャッシュを確かめる
// ANCHOR: dex
// deno run --allow-net=pokeapi.co --allow-read=dex.db --allow-write=dex.db \
//   dex.deno.ts pikachu
import { DatabaseSync } from "node:sqlite";

const API = "https://pokeapi.co/api/v2/";
type Pokemon = { id: number; height: number; weight: number };
type Row = { name: string } & Pokemon;

// 🌐 GET a URL, and read the body as JSON.
// URL を GET して、本文を JSON として読む
async function getJson(url: string): Promise<Pokemon> {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`status ${response.status}`);
  return await response.json();
}

// 🔎 The row of a Pokémon: from dex.db if it is there, or from the web.
// ポケモンの行：dex.db にあればそこから、なければ Web から
async function lookUp(
  db: DatabaseSync,
  name: string,
  fetchJson = getJson,
): Promise<[Row, string]> {
  const find = db.prepare("SELECT * FROM dex WHERE name = ?");
  const cached = find.get(name) as Row | undefined;
  if (cached) return [cached, "cache"];
  const p = await fetchJson(`${API}pokemon/${name}`);
  db.prepare("INSERT INTO dex VALUES (?, ?, ?, ?)")
    .run(name, p.id, p.height, p.weight);
  return [find.get(name) as Row, "web"];
}
// ANCHOR_END: dex

// Stop with an error when a check fails (console.assert only prints).
// 確かめが外れたら、エラーで止まる（console.assert は書くだけ）
function check(ok: boolean, ...what: unknown[]): void {
  if (!ok) throw new Error(`check failed: ${JSON.stringify(what)}`);
}

// The check (1): with no flags, each door says what flag it needs.
// 確かめ（1）：フラグなしでは、どの扉も、要るフラグを言う
async function needs(what: () => unknown): Promise<string> {
  try {
    await what();
    return "ok";
  } catch (e) {
    return `${(e as Error).name}: ${(e as Error).message}`;
  }
}
const net = await needs(() => fetch(`${API}pokemon/pikachu`));
const file = await needs(() => new DatabaseSync("dex.db"));
const env = await needs(() => Deno.env.get("HOME"));
check(net === 'NotCapable: Requires net access to "pokeapi.co:443", ' +
  "run again with the --allow-net flag", net);
check(file.startsWith("NotCapable: Requires read access"), file);
check(env.endsWith("run again with the --allow-env flag"), env);
console.log(net);

// The check (2): the cache, with a database in memory and a fake web.
// 確かめ（2）：メモリーのデータベースと偽物の Web で、キャッシュを
const asked: string[] = [];
const fake = (url: string): Promise<Pokemon> => {
  asked.push(url);
  return Promise.resolve({ id: 25, height: 4, weight: 60 });
};
const db = new DatabaseSync(":memory:");
db.exec("CREATE TABLE dex (name TEXT PRIMARY KEY, id INTEGER," +
  " height INTEGER, weight INTEGER)");
const first = await lookUp(db, "pikachu", fake);
const second = await lookUp(db, "pikachu", fake);
check(first[1] === "web" && second[1] === "cache", first, second);
check(second[0].weight === 60, second);
check(asked.length === 1, asked);
console.log("ok");
