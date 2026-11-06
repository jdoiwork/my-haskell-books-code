// The TypeScript code in the "typed client" chapter of the broker part.
// メッセージブローカーの部の「型のあるクライアント」の章の TypeScript のコード
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

const ties: string[] = [];

// ANCHOR: generic
// TypeScript (generics: the type of the letters is part of the type)
// TypeScript（ジェネリクス：手紙の型が、型の一部になる）
interface Codec<T> {
  toBody(t: T): string;
  fromBody(s: string): T;
}
interface Desk<T> { name: string; codec: Codec<T> }
interface Mailbox<T> { name: string; codec: Codec<T> }

// 🏗️ Tie a desk to a mailbox. Both must carry the same T.
// 仕分け台と郵便受けを結ぶ。両方とも同じ T でなければならない
function bindTo<T>(desk: Desk<T>, key: string, box: Mailbox<T>): void {
  ties.push(`${desk.name} -[${key}]-> ${box.name}`);
}

type Quest = { title: string; reward: number };
type Herb = { herbName: string; bundles: number };

const json = <T>(): Codec<T> => ({
  toBody: (t) => JSON.stringify(t),
  fromBody: (s) => JSON.parse(s) as T,
});
const quests: Desk<Quest> = { name: "quests", codec: json<Quest>() };
const dragons: Mailbox<Quest> = { name: "dragon-team", codec: json() };
const herbs: Mailbox<Herb> = { name: "herb-team", codec: json() };

bindTo(quests, "quest.dragon.*", dragons);
// @ts-expect-error: a desk of quests cannot feed a mailbox of herbs
bindTo(quests, "quest.herb.*", herbs);
// ANCHOR_END: generic

check(ties.length === 2, "both calls ran (types are gone at run time)");
check(ties[0] === "quests -[quest.dragon.*]-> dragon-team", "tie");
const body = quests.codec.toBody({ title: "Slay the dragon", reward: 500 });
check(dragons.codec.fromBody(body).reward === 500, "round trip");

// ANCHOR: pitfall
// TypeScript (the same shape means the same type: structural typing)
// TypeScript（形が同じなら、同じ型：構造的型付け）
type Gold = { amount: number };
type Exp = { amount: number };

const treasury: Desk<Gold> = { name: "treasury", codec: json() };
const training: Mailbox<Exp> = { name: "training", codec: json() };

// No error: Gold and Exp have the same shape, so TypeScript sees one type.
// エラーにならない：Gold と Exp は形が同じなので、同じ型に見える
bindTo(treasury, "gold.#", training);
// ANCHOR_END: pitfall

check(ties[2] === "treasury -[gold.#]-> training", "pitfall tie");
console.log("ok");
