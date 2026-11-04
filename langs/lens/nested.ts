// Changing a nested value without changing the original, for the
// chapter "lens".
// 元を変えずに、入れ子の奥の値を変える。「lens」の章
import { produce } from "immer";

function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

type Stats = { hp: number; mp: number; level: number };
type Hero = { name: string; stats: Stats };
type Party = { leader: Hero; members: Hero[] };

const party: Party = {
  leader: { name: "Lambda", stats: { hp: 30, mp: 10, level: 5 } },
  members: [
    { name: "Python", stats: { hp: 25, mp: 20, level: 4 } },
    { name: "Rust", stats: { hp: 40, mp: 5, level: 6 } },
  ],
};

// ANCHOR: spread
// TypeScript (spread: rebuild every level on the way)
// TypeScript（スプレッド構文：途中の段を、全部作り直す）
const knockedOut: Party = {
  ...party,
  leader: {
    ...party.leader,
    stats: { ...party.leader.stats, hp: 0 },
  },
};
// ANCHOR_END: spread

// ANCHOR: immer
// TypeScript (immer: write as if changing in place; a new value comes out)
// TypeScript（immer：その場で書き換えるように書くと、新しい値が出てくる）
const leveledUp = produce(party, (draft) => {
  for (const member of draft.members) {
    member.stats.level += 1;
  }
});
// ANCHOR_END: immer

// ANCHOR: optional
// TypeScript (optional chaining: read deep, undefined if missing)
// TypeScript（オプショナルチェーン：深く読む。なければ undefined）
const firstName = party.members[0]?.name;          // "Python"
const nobody = ({ ...party, members: [] } as Party).members[0]?.name;
// ANCHOR_END: optional

check(party.leader.stats.hp === 30, "the original is unchanged");
check(knockedOut.leader.stats.hp === 0, "spread");
check(leveledUp.members.map((m) => m.stats.level).join() === "5,7", "immer");
check(party.members[0]!.stats.level === 4, "immer left the original");
check(firstName === "Python" && nobody === undefined, "optional chaining");
console.log("ok");
