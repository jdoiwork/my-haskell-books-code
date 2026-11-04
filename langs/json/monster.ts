// The TypeScript code in the JSON chapter.
// JSON を扱う章の TypeScript のコード
const text = '{"name": "Slime", "hp": 30, "element": "Water", "drops": ["gel"]}';
// ANCHOR: zod
// TypeScript (zod)
// TypeScript（zod を使う）
import { z } from "zod";

const Monster = z.object({
  name: z.string(),
  hp: z.number().int(),
  element: z.enum(["Fire", "Water", "Grass"]),
  drops: z.array(z.string()),
});
type Monster = z.infer<typeof Monster>;

const monster = Monster.parse(JSON.parse(text));
// ANCHOR_END: zod
if (monster.hp !== 30 || monster.element !== "Water") throw new Error("failed: zod");
let failed = false;
try {
  Monster.parse(JSON.parse('{"name": "Slime", "hp": 30, "element": "Fier", "drops": []}'));
} catch {
  failed = true;
}
if (!failed) throw new Error("failed: zod should reject Fier");
// ANCHOR: code
// TypeScript
type Action =
  | { type: "attack"; value: number }
  | { type: "spell"; value: "Fire" | "Water" | "Grass" }
  | { type: "heal"; value: number }
  | { type: "defend" };
// ANCHOR_END: code
const actions: Action[] = [
  { type: "attack", value: 30 },
  { type: "spell", value: "Fire" },
  { type: "defend" },
];
if (actions.length !== 3) throw new Error("failed: Action");
