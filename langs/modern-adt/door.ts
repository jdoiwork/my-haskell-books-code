// The door of the training in the chapter "ADTs are everywhere".
// 「代数的データ型は、どこにでもある」の章のトレーニングの扉
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: door
// TypeScript (only a locked door has a key name)
// TypeScript（鍵の名前を持つのは、鍵のかかった扉だけ）
type Door =
  | { kind: "open" }
  | { kind: "closed" }
  | { kind: "locked"; key: string };

function tryOpen(key: string, door: Door): Door {
  switch (door.kind) {
    case "open":
    case "closed":
      return { kind: "open" };
    case "locked":
      return key === door.key ? { kind: "open" } : door;
  }
}
// ANCHOR_END: door

check(tryOpen("silver", { kind: "locked", key: "silver" }).kind === "open", "1");
check(tryOpen("gold", { kind: "locked", key: "silver" }).kind === "locked", "2");
check(tryOpen("gold", { kind: "closed" }).kind === "open", "3");
console.log("ok");
