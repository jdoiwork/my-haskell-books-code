// ANCHOR: all
// 📊 The bar of the progress: the numbers the core counted, drawn by
//    Mantine's Progress. The same numbers the terminal draws with ▓.
// 進み具合のバー：核が数えた数を、Mantine の Progress が描く。ターミナルが
// ▓ で描くのと同じ数
import { Group, Progress, Text } from "@mantine/core";

import type { ProgressNumbers } from "./tsundoku-api";

export function Bar({ p }: { p: ProgressNumbers }) {
  return (
    <Group gap="xs" mt="xs" wrap="nowrap">
      <Progress value={p.percent} flex={1} size="md" radius="xl"
                aria-label={`${p.percent}%`} />
      <Text size="xs" c="dimmed" w="3em" ta="right">{p.percent}%</Text>
    </Group>
  );
}
// ANCHOR_END: all
