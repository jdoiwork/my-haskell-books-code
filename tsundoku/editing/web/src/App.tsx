// ANCHOR: all
// The screen: the shelf, one card, the form for a new work, or the
// screen to fix a work. Which one is the only state here.
// 画面：本棚か、カード1枚か、新しい作品のフォームか、作品を直す画面か。
// ここにある状態は、どれを見せるかだけ
import { useState } from "react";

import { CardView } from "./CardView";
import { EditWork } from "./EditWork";
import { NewWork } from "./NewWork";
import { WorkList } from "./WorkList";

// ANCHOR: screens
type Screen =
  | { on: "shelf" }
  | { on: "card"; id: number }
  | { on: "new" }
  | { on: "edit"; id: number };

export function App() {
  const [screen, setScreen] = useState<Screen>({ on: "shelf" });
  const open = (id: number) => setScreen({ on: "card", id });
  return (
    <>
      <header>
        <a href="#" onClick={() => setScreen({ on: "shelf" })}>📚 tsundoku</a>
        <a href="#" className="add"
           onClick={() => setScreen({ on: "new" })}>＋ 本を足す</a>
      </header>
      <main>
        {screen.on === "shelf" && <WorkList onOpen={open} />}
        {screen.on === "card" &&
          <CardView id={screen.id} onEdit={() =>
                      setScreen({ on: "edit", id: screen.id })} />}
        {screen.on === "new" && <NewWork onAdded={open} />}
        {screen.on === "edit" &&
          <EditWork id={screen.id} onDone={() => open(screen.id)} />}
      </main>
    </>
  );
}
// ANCHOR_END: screens
// ANCHOR_END: all
