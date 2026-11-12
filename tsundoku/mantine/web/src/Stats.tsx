// ANCHOR: all
// The statistics of a year: three numbers on top, the pages read month
// by month (a bar chart), the kinds on the shelf (a donut), the tag
// cloud, and the year's best. Every number comes from the API (the
// core counted them); the charts only draw them.
// 1年の統計：上に3つの数、月ごとに読んだページ（棒グラフ）、本棚の種類
// （ドーナツ）、タグの雲、その年のベスト。数は全部 API から（数えたのは
// 核）。グラフは描くだけ
import { BarChart, DonutChart } from "@mantine/charts";
import {
  ActionIcon, Badge, Button, Group, Paper, Rating, SimpleGrid, Stack, Table,
  Text, Title,
} from "@mantine/core";
import { IconChevronLeft, IconChevronRight } from "@tabler/icons-react";
import { useEffect, useState } from "react";

import { shelf } from "./api";
import { Problems } from "./NewWork";
import { cloudSize, kindSlices, monthRows } from "./charts";
import { halfStars } from "./shadows";
import { tone } from "./theme";
import type { StatsItem, TagCount } from "./tsundoku-api";

// ANCHOR: stats
export function Stats({ onTag, onOpen }: {
  onTag: (tag: string) => void;
  onOpen: (id: number) => void;
}) {
  const [year, setYear] = useState(() => new Date().getFullYear());
  const [stats, setStats] = useState<StatsItem | null>(null);
  const [cloud, setCloud] = useState<TagCount[]>([]);
  const [why, setWhy] = useState<string[]>([]);
  useEffect(() => {
    shelf.stats(year).then((a) => {
      if (a.ok) { setStats(a.value); setWhy([]); }
      else setWhy(a.problem.messages);
    });
    shelf.tags().then((a) => { if (a.ok) setCloud(a.value); });
  }, [year]);

  if (stats === null) return why.length > 0
    ? <Problems messages={why} />
    : <Text>読みこみ中…</Text>;
  const sum = (f: (m: StatsItem["months"][number]) => number) =>
    stats.months.reduce((n, m) => n + f(m), 0);
  const works = stats.kinds.reduce((n, k) => n + k.works, 0);
  return (
    <Stack>
      <Group justify="space-between">
        <Title order={2}>読書の統計</Title>
        <Group gap="xs">
          <ActionIcon variant="default" aria-label="前の年"
                      onClick={() => setYear(year - 1)}>
            <IconChevronLeft size={16} />
          </ActionIcon>
          <Text fw={700}>{year}年</Text>
          <ActionIcon variant="default" aria-label="次の年"
                      onClick={() => setYear(year + 1)}>
            <IconChevronRight size={16} />
          </ActionIcon>
        </Group>
      </Group>
      <SimpleGrid cols={{ base: 1, sm: 3 }}>
        <BigNumber label="読み終えた作品" unit="作品"
                   value={sum((m) => m.finished)} />
        <BigNumber label="読んだページ" unit="ページ"
                   value={sum((m) => m.pages)} />
        <BigNumber label="タグ" value={cloud.length} unit="種類" />
      </SimpleGrid>
      {/* ANCHOR: charts */}
      <Paper withBorder p="md">
        <Text fw={700} mb="sm">月ごとに読んだページ</Text>
        <BarChart h={240} dataKey="month" data={monthRows(year, stats.months)}
                  series={[{ name: "pages", label: "ページ",
                             color: "indigo.6" }]} />
      </Paper>
      <SimpleGrid cols={{ base: 1, sm: 2 }}>
        <Paper withBorder p="md">
          <Text fw={700} mb="sm">本棚の種類</Text>
          <Group justify="center">
            <DonutChart data={kindSlices(stats.kinds)} withLabels
                        withLabelsLine labelsType="value" withTooltip
                        chartLabel={`${works} 作品`} />
          </Group>
          <Group justify="center" gap="xs" mt="xs">
            {kindSlices(stats.kinds).map((s) => (
              <Badge key={s.name} color={s.color} variant="dot">
                {s.name}
              </Badge>))}
          </Group>
        </Paper>
        <Paper withBorder p="md">
          <Text fw={700} mb="sm">タグの雲</Text>
          <Group gap="xs">
            {cloud.map((t) => (
              <Badge key={t.tag} size={cloudSize(t.works, cloud)}
                     variant="light" tt="none" style={{ cursor: "pointer" }}
                     onClick={() => onTag(t.tag)}>#{t.tag}</Badge>))}
          </Group>
        </Paper>
      </SimpleGrid>
      {/* ANCHOR_END: charts */}
      <Paper withBorder p="md">
        <Text fw={700} mb="sm">{year}年のベスト</Text>
        {stats.best.length === 0
          ? <Text c="dimmed">この年に読み終えた作品は、まだありません</Text>
          : <Table>
              <Table.Tbody>
                {stats.best.map((w, i) => (
                  <Table.Tr key={w.id}>
                    <Table.Td w={40}>{i + 1}</Table.Td>
                    <Table.Td>
                      <Button variant="subtle" size="compact-sm"
                              onClick={() => onOpen(w.id)}>{w.title}</Button>
                    </Table.Td>
                    <Table.Td>
                      {w.rating !== null &&
                        <Rating value={halfStars(w.rating)} fractions={2}
                                readOnly color={tone.star} />}
                    </Table.Td>
                  </Table.Tr>))}
              </Table.Tbody>
            </Table>}
      </Paper>
    </Stack>
  );
}
// ANCHOR_END: stats

// 🔢 One big number with its label.
// 大きな数1つと、その名前
function BigNumber({ label, value, unit }: {
  label: string; value: number; unit: string;
}) {
  return (
    <Paper withBorder p="md">
      <Text size="sm" c="dimmed">{label}</Text>
      <Text fz={32} fw={700}>{value}
        <Text span size="sm" c="dimmed"> {unit}</Text></Text>
    </Paper>
  );
}
// ANCHOR_END: all
