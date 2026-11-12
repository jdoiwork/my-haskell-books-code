// ANCHOR: all
// The shelf: the tags to choose from, a search box, and a card for each
// work (all of them, or those with the chosen tag).
// 本棚：選べるタグ、探す箱、作品ごとにカード1枚（全部か、選んだタグの
// 作品だけ）
// ANCHOR: imports
import {
  Anchor, Badge, Card, Group, Mark, Paper, Rating, SimpleGrid, Stack, Text,
  TextInput, Title,
} from "@mantine/core";
import { IconSearch } from "@tabler/icons-react";
import { useEffect, useRef, useState } from "react";

import { shelf } from "./api";
import { Bar } from "./Bar";
import { Problems } from "./NewWork";
import { halfStars, stateColor, stateLabel } from "./shadows";
import { tone } from "./theme";
import type { HitItem, TagCount, WorkSummary } from "./tsundoku-api";
// ANCHOR_END: imports

// ANCHOR: list
export function WorkList({ tag, onTag, onOpen }: {
  tag: string | null;
  onTag: (tag: string | null) => void;
  onOpen: (id: number) => void;
}) {
  const [works, setWorks] = useState<WorkSummary[]>([]);
  const [cloud, setCloud] = useState<TagCount[]>([]);
  // why the shelf could not be read: an empty grid alone would look
  // like an empty shelf
  // 本棚が読めなかった理由。空の並びだけだと、空の本棚に見えてしまう
  const [why, setWhy] = useState<string[]>([]);
  useEffect(() => {
    // an answer for a tag chosen before comes late: drop it (A, then B)
    // 前に選んだタグの答えが遅れて届いたら、捨てる（A のあと B）
    let stale = false;
    const asked = tag === null ? shelf.listWorks() : shelf.tagged(tag);
    asked.then((a) => {
      if (stale) return;
      if (a.ok) { setWorks(a.value); setWhy([]); }
      else { setWorks([]); setWhy(a.problem.messages); }
    });
    shelf.tags().then((a) => { if (!stale && a.ok) setCloud(a.value); });
    return () => { stale = true; };
  }, [tag]);
  return (
    <Stack>
      <Title order={2}>
        {tag === null ? "本棚" : `タグ「${tag}」の本棚`}
      </Title>
      <TagPicker cloud={cloud} chosen={tag} onTag={onTag} />
      <Search onOpen={onOpen} />
      {why.length > 0 && <Problems messages={why} />}
      <SimpleGrid cols={{ base: 1, sm: 2 }}>
        {works.map((w) => <WorkCard key={w.id} w={w} onOpen={onOpen} />)}
      </SimpleGrid>
    </Stack>
  );
}
// ANCHOR_END: list

// ANCHOR: workCard
// 🗂️ One work as a card: title, creator, the state's badge, the stars,
//    and the bar. Click anywhere on it to open it.
// 作品1つをカードに：題、作者、状態の札、★、バー。どこを押しても開く
function WorkCard({ w, onOpen }: {
  w: WorkSummary;
  onOpen: (id: number) => void;
}) {
  return (
    <Card withBorder shadow="sm" padding="md" component="button"
          onClick={() => onOpen(w.id)} style={{ textAlign: "left" }}>
      <Group justify="space-between" wrap="nowrap" align="flex-start">
        <Text fw={700} lineClamp={2}>{w.title}</Text>
        <Badge color={stateColor(w.reading)} variant="light">
          {stateLabel(w.reading)}
        </Badge>
      </Group>
      <Text size="sm" c="dimmed">{w.creator}</Text>
      <Group mt="xs" justify="space-between">
        {w.rating === null
          ? <Text size="sm" c="dimmed">まだ評価なし</Text>
          : <Rating value={halfStars(w.rating)} fractions={2} readOnly
                    color={tone.star} />}
      </Group>
      {w.progress && <Bar p={w.progress} />}
    </Card>
  );
}
// ANCHOR_END: workCard

// ANCHOR: tagPicker
// 🏷️ The tags as badges: click one to see only its works, click it
//    again (or "すべて") to see them all.
// タグを札で：押すと、そのタグの作品だけ。もう一度押すか「すべて」で、全部
function TagPicker({ cloud, chosen, onTag }: {
  cloud: TagCount[];
  chosen: string | null;
  onTag: (tag: string | null) => void;
}) {
  if (cloud.length === 0) return null;
  const badge = (label: string, value: string | null, works?: number) => (
    <Badge key={label} size="lg" tt="none" style={{ cursor: "pointer" }}
           variant={chosen === value ? "filled" : "light"}
           onClick={() => onTag(chosen === value ? null : value)}>
      {label}{works !== undefined && ` ${works}`}
    </Badge>
  );
  return (
    <Group gap="xs">
      {badge("すべて", null)}
      {cloud.map((t) => badge(`#${t.tag}`, t.tag, t.works))}
    </Group>
  );
}
// ANCHOR_END: tagPicker

// ANCHOR: search
// 🔎 Search the shelf. The matched pieces are <Mark>ed here, by the
//    screen; the API only said which pieces matched.
// 本棚を探す。当たったかけらに <Mark> を付けるのは、ここ（画面）。
// API は、どのかけらが当たったかを言っただけ
function Search({ onOpen }: { onOpen: (id: number) => void }) {
  const [q, setQ] = useState("");
  const [hits, setHits] = useState<HitItem[]>([]);
  const [why, setWhy] = useState<string[]>([]);
  // only the newest search counts: an older answer that comes late
  //   is dropped (the same as the card's reading again)
  // いちばん新しく探したものだけを見せる。遅れて届いた古い答えは
  // 捨てる（カードの読み直しと同じ）
  const asked = useRef(0);
  const find = async (e: React.FormEvent) => {
    e.preventDefault();
    const mine = ++asked.current;
    const a = await shelf.search(q);
    if (mine !== asked.current) return;
    if (a.ok) { setHits(a.value); setWhy([]); }
    else { setHits([]); setWhy(a.problem.messages); }
  };
  return (
    <form onSubmit={find}>
      <TextInput value={q} placeholder="本棚の中を探す"
                 leftSection={<IconSearch size={16} />}
                 onChange={(e) => setQ(e.currentTarget.value)} />
      <Problems messages={why} />
      <Stack gap="xs" mt={hits.length > 0 ? "sm" : 0}>
        {hits.map((h, i) => (
          <Paper key={i} withBorder p="sm">
            <Anchor component="button" type="button" fw={700}
                    onClick={() => onOpen(h.work)}>{h.title}</Anchor>
            {/* a spoiler is hidden in the hits too, as on the card */}
            {/* ネタバレは、カードと同じく、ヒットでも伏せる */}
            {h.spoiler
              ? <Text size="sm" c="dimmed" fs="italic">
                  （ネタバレの感想に当たりました。伏せています）</Text>
              : <Text size="sm">{h.snippet.map((s, j) =>
                  s.match ? <Mark key={j} color={tone.mark}>{s.text}</Mark>
                          : s.text)}</Text>}
          </Paper>
        ))}
      </Stack>
    </form>
  );
}
// ANCHOR_END: search
// ANCHOR_END: all
