// ANCHOR: all
// One quest as a card. A finished quest gets a green badge.
// 依頼1つ分のカード。終わった依頼には、緑の札が付く
import { Badge, Button, Card, Group, Text } from "@mantine/core";
import { IconCheck, IconCoin } from "@tabler/icons-react";

import type { Quest } from "./types";

type Props = { quest: Quest; onFinish: (id: number) => void };

export function QuestCard({ quest, onFinish }: Props) {
  return (
    <Card withBorder shadow="sm" padding="md">
      <Group justify="space-between">
        <div>
          <Text fw={600} td={quest.done ? "line-through" : undefined}>
            {quest.title}
          </Text>
          <Group gap={4} c="dimmed">
            <IconCoin size={14} />
            <Text size="sm">{quest.reward} G</Text>
          </Group>
        </div>
        {quest.done ? (
          <Badge color="green" leftSection={<IconCheck size={12} />}>
            Done
          </Badge>
        ) : (
          <Button variant="light" onClick={() => onFinish(quest.questId)}>
            Finish
          </Button>
        )}
      </Group>
    </Card>
  );
}
// ANCHOR_END: all
