// Typed keys in TypeScript, for the chapter "A client strict about types".
// 「型に厳しいクライアント」の章の型付きのキーを、TypeScript で
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: keys
// TypeScript (a phantom type parameter on the key)
// TypeScript（キーに、中身の型の印を付ける）
// A Map stands in for the server.
// サーバーの代わりの Map
const store = new Map<string, string>();

// decode gives undefined when the text is not a value of type T.
// decode は、文字列が T の値として読めなければ undefined
type Codec<T> = {
  encode: (v: T) => string;
  decode: (s: string) => T | undefined;
};
type Key<T> = { name: string; codec: Codec<T> };

const num: Codec<number> = {
  encode: String,
  // Number("high") is NaN, and NaN is still a number: check it.
  // Number("high") は NaN で、NaN も number のうち：自分で確かめる
  decode: (s) => (/^-?\d+$/.test(s) ? Number(s) : undefined),
};
const text: Codec<string> = { encode: (s) => s, decode: (s) => s };

function get<T>(key: Key<T>): T | null {
  const raw = store.get(key.name);
  if (raw === undefined) return null;
  const value = key.codec.decode(raw);
  if (value === undefined) throw new Error(`not decodable: ${raw}`);
  return value;
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
// The mistake ran anyway at run time: hero was missing, so it became "1".
check(store.get("hero") === "1", "the mistake ran anyway at run time");
store.set("gold", "high");
let caught = false;
try { get(gold); } catch { caught = true; }
check(caught, "a value that does not decode is noticed");
console.log("ok");
