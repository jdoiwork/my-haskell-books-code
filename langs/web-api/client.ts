// The TypeScript client in the Web API chapter (for the React side).
// Web API の章の TypeScript のクライアント（React 側で使う）
import type { Adventurer, ApiError, NewAdventurer } from "./guild-types.ts";

// ANCHOR: client
// TypeScript (calling the Haskell API with types made from Haskell)
// TypeScript（Haskell から作った型で、Haskell の API を呼ぶ）
// Same origin: the dev server passes /adventurers on to Haskell (port 8080).
// 同じ Origin：開発サーバーが /adventurers を Haskell（8080 番）へ中継する
const base = "";

export async function listAdventurers(): Promise<Adventurer[]> {
  const res = await fetch(`${base}/adventurers`);
  return (await res.json()) as Adventurer[];
}

export async function addAdventurer(
  newOne: NewAdventurer,
): Promise<Adventurer | ApiError> {
  const res = await fetch(`${base}/adventurers`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(newOne),
  });
  return (await res.json()) as Adventurer | ApiError;
}
// ANCHOR_END: client

// @ts-expect-error: "lv" is not a field of NewAdventurer.
const typo: NewAdventurer = { name: "Lambda", job: "Mage", lv: 3 };
void typo;

// ANCHOR: levelup
// TypeScript (the answer: one level up for this name)
// TypeScript（解答：この名前の冒険者のレベルを1上げる）
export async function levelUp(name: string): Promise<Adventurer | ApiError> {
  const res = await fetch(
    `${base}/adventurers/${encodeURIComponent(name)}/levelup`,
    { method: "POST" },
  );
  return (await res.json()) as Adventurer | ApiError;
}
// ANCHOR_END: levelup
