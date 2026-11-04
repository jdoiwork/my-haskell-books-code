// Typed keys in TypeScript, for the chapter "A client strict about types".
// 「型に厳しいクライアント」の章の型付きのキーを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const store = new Map<string, string>();   // stands in for the server

// ANCHOR: keys
// TypeScript (a phantom type parameter on the key)
// TypeScript（キーに、中身の型の印を付ける）
type Codec<T> = { encode: (v: T) => string; decode: (s: string) => T };
type Key<T> = { name: string; codec: Codec<T> };

const num: Codec<number> = { encode: String, decode: Number };
const text: Codec<string> = { encode: (s) => s, decode: (s) => s };

function get<T>(key: Key<T>): T | null {
  const raw = store.get(key.name);
  return raw === undefined ? null : key.codec.decode(raw);
}

function incr(key: Key<number>): number {
  const next = (get(key) ?? 0) + 1;
  store.set(key.name, num.encode(next));
  return next;
}

const gold: Key<number> = { name: "gold", codec: num };
const hero: Key<string> = { name: "hero", codec: text };
incr(gold);
// @ts-expect-error: hero holds text, not a number
incr(hero);
// ANCHOR_END: keys

check(get(gold) === 1, "gold");
check(store.get("hero") === "1", "the mistake ran anyway at run time");
console.log("ok");
