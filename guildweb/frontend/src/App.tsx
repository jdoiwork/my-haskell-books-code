// ANCHOR: all
// The whole page: the header, the form and the list of quests.
// ページ全体：見出し、フォーム、依頼の一覧
import { useEffect, useState } from "react";
import {
  ActionIcon, Alert, Container, Group, SimpleGrid, Stack, Text, Title,
  useMantineColorScheme,
} from "@mantine/core";
import { IconMoon, IconSword, IconSun } from "@tabler/icons-react";

import { fetchQuests, finishQuest, postQuest } from "./api";
import { QuestCard } from "./QuestCard";
import { QuestForm } from "./QuestForm";
import type { NewQuest, Quest } from "./types";

export function App() {
  const [quests, setQuests] = useState<Quest[]>([]);
  const [error, setError] = useState<string | null>(null);
  const { colorScheme, toggleColorScheme } = useMantineColorScheme();

  // 📥 Load the quests once, when the page first appears. Keep the
  //    quests posted while it was loading (they are not in `loaded`).
  // ページが最初に出たときに、1回だけ依頼を読みこむ。読みこみの途中で
  // 貼った依頼（loaded にないもの）は残す
  useEffect(() => {
    fetchQuests()
      .then((loaded) =>
        setQuests((now) => [
          ...loaded,
          ...now.filter((q) => !loaded.some((l) => l.questId === q.questId)),
        ]),
      )
      .catch((e) => setError(e.message));
  }, []);

  // 📝 Post a quest, then add the answer from the server to the list.
  //    Says whether it worked, so the form keeps the text on a failure.
  // 依頼を貼り、サーバーから返ってきた依頼を一覧に足す。
  // うまくいったかを返す（失敗なら、フォームは入力を残す）
  const post = async (quest: NewQuest) => {
    try {
      const added = await postQuest(quest);
      setQuests((now) => [...now, added]);
      setError(null);
      return true;
    } catch (e) {
      setError((e as Error).message);
      return false;
    }
  };

  // ✅ Finish a quest: replace it in the list with the server's answer.
  // 依頼を終える：一覧の中のその依頼を、サーバーの答えで置きかえる
  const finish = async (id: number) => {
    try {
      const finished = await finishQuest(id);
      setQuests((now) => now.map((q) => (q.questId === id ? finished : q)));
      setError(null);
    } catch (e) {
      setError((e as Error).message);
    }
  };

  const open = quests.filter((q) => !q.done);
  const gold = open.reduce((sum, q) => sum + q.reward, 0);

  return (
    <Container size="sm" py="xl">
      <Stack gap="lg">
        <Group justify="space-between">
          <Group gap="xs">
            <IconSword size={28} />
            <Title order={2}>Guild Quest Board</Title>
          </Group>
          <ActionIcon variant="default" size="lg" onClick={toggleColorScheme}>
            {colorScheme === "dark"
              ? <IconSun size={18} />
              : <IconMoon size={18} />}
          </ActionIcon>
        </Group>
        <QuestForm onPost={post} />
        {error && <Alert color="red" title="Oops">{error}</Alert>}
        <Text c="dimmed">
          {open.length} open quests · {gold} G waiting
        </Text>
        <SimpleGrid cols={{ base: 1, sm: 2 }}>
          {quests.map((quest) => (
            <QuestCard key={quest.questId} quest={quest} onFinish={finish} />
          ))}
        </SimpleGrid>
      </Stack>
    </Container>
  );
}
// ANCHOR_END: all
