// State as a reducer, for the chapter "Reader, Writer and State".
// State を reducer で。「Reader・Writer・State」の章
function check(ok: boolean, what: string): void {
  if (!ok) throw new Error(`failed: ${what}`);
}

// ANCHOR: procedural
// TypeScript (procedural style: an object changed in place)
// TypeScript（手続き型スタイル：オブジェクトをその場で書き換える）
class Counter {
  hp = 30;
  turns = 0;
  hit(damage: number): void {
    this.hp -= damage;
    this.turns += 1;
  }
}

const counter = new Counter();
for (const damage of [7, 9]) {
  counter.hit(damage);
}
// ANCHOR_END: procedural

// ANCHOR: functional
// TypeScript (functional style: a reducer, like React's useReducer)
// TypeScript（関数型スタイル：React の useReducer のような reducer）
type Arena = { hp: number; turns: number };
type Action = { kind: "hit"; damage: number };

function reducer(state: Arena, action: Action): Arena {
  switch (action.kind) {
    case "hit":
      return { hp: state.hp - action.damage, turns: state.turns + 1 };
  }
}

const actions: Action[] = [
  { kind: "hit", damage: 7 },
  { kind: "hit", damage: 9 },
];
const final = actions.reduce(reducer, { hp: 30, turns: 0 });
// ANCHOR_END: functional

check(counter.hp === 14 && counter.turns === 2, "procedural");
check(final.hp === 14 && final.turns === 2, "reducer");
console.log("ok");
