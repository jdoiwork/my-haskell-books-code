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

  // 📥 Load the quests once, when the page first appears.
  // ページが最初に出たときに、1回だけ依頼を読みこむ
  useEffect(() => {
    fetchQuests().then(setQuests).catch((e) => setError(e.message));
  }, []);

  // 📝 Post a quest, then add the answer from the server to the list.
  // 依頼を貼り、サーバーから返ってきた依頼を一覧に足す
  const post = async (quest: NewQuest) => {
    try {
      const added = await postQuest(quest);
      setQuests([...quests, added]);
      setError(null);
    } catch (e) {
      setError((e as Error).message);
    }
  };

  // ✅ Finish a quest: replace it in the list with the server's answer.
  // 依頼を終える：一覧の中のその依頼を、サーバーの答えで置きかえる
  const finish = async (id: number) => {
    const finished = await finishQuest(id);
    setQuests(quests.map((q) => (q.questId === id ? finished : q)));
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
