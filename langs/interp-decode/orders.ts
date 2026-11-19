// The orders of the web shop, as in interp-decode.md, with zod.
// 「データを読む」の章の、ウェブのお店の注文を zod で
// ANCHOR: zod
// TypeScript (zod): the shape is a value, and the type comes from it
// TypeScript（zod）：形は値で、型はそこから作る
import { z } from "zod";

const Order = z.object({
  item: z.string(),
  count: z.number().int(),
  price: z.number().int(),
});
const Orders = z.object({ orders: z.array(Order) });
type Orders = z.infer<typeof Orders>;

const bad = JSON.parse(`{"orders": [
  {"item": "sticker", "count": "four", "price": 300.5},
  {"item": "mug", "count": 1, "price": 1200},
  {"name": "T-shirt, large", "count": 2}]}`);

const result = Orders.safeParse(bad);
if (result.success) {
  const orders: Orders = result.data;
  console.log(orders.orders.length);
} else {
  for (const issue of result.error.issues) {
    console.log(issue.path.join("."), issue.message);
  }
}
// ANCHOR_END: zod

if (result.success) throw new Error("failed: zod");
const paths = result.error.issues.map((i) => i.path.join("."));
const want = "orders.0.count orders.0.price orders.2.item orders.2.price";
if (paths.join(" ") !== want) {
  throw new Error("failed: paths " + paths.join(" "));
}
