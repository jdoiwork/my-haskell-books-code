// The TypeScript code in the chapter on IO: one way of writing for each
// kind of box. ?. and ?? for a value that may be missing, and await
// for a Promise. Lam's do works for every Monad instead.
// 入出力の章の TypeScript のコード：箱の種類ごとに、別の書き方。
// ないかもしれない値には ?. と ??、Promise には await。Lam の do は、
// Monad の仲間なら、どれにも使える

type Order = [string, number];

// ANCHOR: optional
// TypeScript (?. and ??): for a value that may be missing
// TypeScript（?. と ??）：ないかもしれない値には、これ
function orderOf(j: unknown): Order | undefined {
  const o = j as { item?: unknown; count?: unknown } | null;
  const item = o?.item;
  const count = o?.count;
  if (typeof item !== "string" || typeof count !== "number") {
    return undefined;
  }
  return [item, count];
}
// ANCHOR_END: optional

// ANCHOR: await
// TypeScript (async and await): for a Promise, an action that is done
// later. read is passed in, so a test can give a fake one.
// TypeScript（async と await）：Promise（あとでやる頼みごと）には、
// これ。read は引数で受け取るので、テストでは偽物を渡せる
async function ordersIn(
  read: (path: string) => Promise<string>,
): Promise<Order[] | undefined> {
  const text = await read("orders.json");
  const items: unknown = JSON.parse(text);
  if (!Array.isArray(items)) return undefined;
  const orders = items.map(orderOf);
  return orders.every((o) => o !== undefined) ? orders : undefined;
}
// ANCHOR_END: await

// A fake disk, as the fake world of Lam.
// 偽物のディスク。Lam の偽物の世界と同じ
const disk: Record<string, string> = {
  "orders.json": '[{"item": "sticker", "count": 4}, {"item": "mug", "count": 1}]',
};
const fakeRead = async (path: string): Promise<string> => disk[path] ?? "";

ordersIn(fakeRead).then((orders) => {
  const expected = [["sticker", 4], ["mug", 1]];
  if (JSON.stringify(orders) !== JSON.stringify(expected)) {
    throw new Error(`unexpected: ${JSON.stringify(orders)}`);
  }
  if (orderOf({ item: "mug" }) !== undefined || orderOf(null) !== undefined) {
    throw new Error("a missing field must give undefined");
  }
  console.log(JSON.stringify(orders));
});
