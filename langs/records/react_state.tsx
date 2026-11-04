// The React code in the records chapter (type-checked only: it runs in a browser).
// レコードと newtype の章の React のコード（ブラウザで動くので、型だけを確かめる）
import { useState } from "react";

type Element = "fire" | "water" | "grass";
type Monster = { name: string; hp: number; element: Element };
const slime: Monster = { name: "Slime", hp: 30, element: "water" };

// ANCHOR: useState
// TypeScript (React)
// TypeScript（React）
export function MonsterCard() {
  const [monster, setMonster] = useState<Monster>(slime);

  // ❌ Change the object in place: it is the same object,
  //    so React does not notice and the screen stays the same.
  // ❌ その場で書き換える：同じオブジェクトなので、
  //    React は気づかず、画面が変わらない
  const hitInPlace = () => {
    monster.hp = Math.max(0, monster.hp - 10);
    setMonster(monster);
  };

  // ✅ Make a new object with only hp changed.
  // ✅ hp だけを変えた、新しいオブジェクトを作る
  const hit = () => {
    setMonster({ ...monster, hp: Math.max(0, monster.hp - 10) });
  };

  return (
    <div>
      <p>{monster.name}: HP {monster.hp}</p>
      <button onClick={hit}>Attack</button>
      <button onClick={hitInPlace}>Attack (in place)</button>
    </div>
  );
}
// ANCHOR_END: useState
