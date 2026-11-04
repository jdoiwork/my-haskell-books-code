// ANCHOR: all
// Step 2: open quests first, and larger rewards first among them.
// ステップ2：終わっていない依頼を先に。その中では報酬の大きい順に
import type { Quest } from "../types";

export const sortQuests = (quests: Quest[]): Quest[] =>
  [...quests].sort(
    (a, b) => Number(a.done) - Number(b.done) || b.reward - a.reward,
  );
// ANCHOR_END: all
