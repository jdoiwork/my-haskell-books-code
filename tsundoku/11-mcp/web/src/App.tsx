// ANCHOR: all
// The screen: the shelf, or one card. Which one is the only state here.
// 画面：本棚か、カード1枚か。ここにある状態は、どちらを見せるかだけ
import { useState } from "react";

import { CardView } from "./CardView";
import { WorkList } from "./WorkList";

export function App() {
  const [opened, setOpened] = useState<number | null>(null);
  return (
    <>
      <header>
        <a href="#" onClick={() => setOpened(null)}>📚 tsundoku</a>
      </header>
      <main>
        {opened === null
          ? <WorkList onOpen={setOpened} />
          : <CardView id={opened} />}
      </main>
    </>
  );
}
// ANCHOR_END: all
