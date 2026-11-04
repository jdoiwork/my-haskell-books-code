// ANCHOR: all
// A form to post a quest: a title, a reward, and a button.
// 依頼を貼るフォーム：題と報酬と、ボタン
import { useState } from "react";
import { Button, Group, NumberInput, TextInput } from "@mantine/core";
import { IconPlus } from "@tabler/icons-react";

import type { NewQuest } from "./types";

// 🧩 Props: what the parent gives this component.
// props：親がこの部品に渡すもの
type Props = { onPost: (quest: NewQuest) => Promise<void> };

export function QuestForm({ onPost }: Props) {
  // 🗃️ State: what this component remembers between draws.
  // 状態：この部品が、描き直しのあいだも覚えておくもの
  const [title, setTitle] = useState("");
  const [reward, setReward] = useState<number>(50);

  const submit = async () => {
    await onPost({ title, reward });
    setTitle("");
  };

  return (
    <Group align="end">
      <TextInput
        label="Quest"
        placeholder="slay the slime"
        value={title}
        onChange={(event) => setTitle(event.currentTarget.value)}
        style={{ flex: 1 }}
      />
      <NumberInput
        label="Reward (G)"
        value={reward}
        onChange={(value) => setReward(Number(value))}
        min={0}
        max={10000}
        w={130}
      />
      <Button leftSection={<IconPlus size={16} />} onClick={submit}>
        Post
      </Button>
    </Group>
  );
}
// ANCHOR_END: all
