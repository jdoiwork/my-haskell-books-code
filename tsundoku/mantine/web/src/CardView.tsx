// ANCHOR: all
// One card: the title and its tags, the state and its buttons, the
// stars, the bar; then tabs for the table, the notes (as I may see them
// now), and my own attributes; at the side, the box of tags and the
// works alike. After each change, read the card again from the API:
// the screen shows what is on the shelf.
// カード1枚：題とタグ、状態とボタン、★、バー。その下に、目次・感想（いまの
// 自分に見せてよいもの）・自分で決めた属性のタブ。横に、タグの箱と似ている
// 作品。変えるたびに、API からカードを読み直す。画面は、本棚にあるとおりを
// 見せる
// ANCHOR: imports
import {
  Badge, Button, Grid, Group, Paper, Rating, Stack, Switch, Tabs, TagsInput,
  Text, Textarea, Timeline, Title,
} from "@mantine/core";
import {
  IconLock, IconMessage, IconPencil, IconTag,
} from "@tabler/icons-react";
import { useCallback, useEffect, useRef, useState } from "react";

import { type Answer, shelf } from "./api";
import { AttrForm } from "./AttrForm";
import { Bar } from "./Bar";
import { dayOf } from "./day";
import { Problems } from "./NewWork";
import { marksFor } from "./outline";
import {
  eventLabel, halfStars, pointsOf, stateColor, stateLabel, whereOf,
} from "./shadows";
import { tone } from "./theme";
import { Toc } from "./Toc";
import type {
  CardDetail, ExtrasItem, FormField, SeenNote, TocItem,
} from "./tsundoku-api";
// ANCHOR_END: imports

// ANCHOR: card
// ANCHOR: props
export function CardView({ id, onEdit, onTag, onOpen }: {
  id: number;
  onEdit: () => void;
  onTag: (tag: string) => void;
  onOpen: (id: number) => void;
}) {
// ANCHOR_END: props
  const [card, setCard] = useState<CardDetail | null>(null);
  const [fields, setFields] = useState<FormField[]>([]);
  const [extras, setExtras] = useState<ExtrasItem | null>(null);
  const [known, setKnown] = useState<string[]>([]);
  const [problem, setProblem] = useState<string[]>([]);

  // ANCHOR: reload
  // 🔁 Read the card again. Two readings can overlap (two quick clicks),
  //    and their answers can come back in either order: show only the
  //    newest one asked, and drop an older answer that comes late.
  // カードを読み直す。読み直しは重なることがあり（すばやく2回押す）、
  // 答えはどちらが先に届くかわからない。いちばん新しく頼んだものだけを
  // 見せ、遅れて届いた古い答えは捨てる
  const asked = useRef(0);
  const reload = useCallback(async () => {
    const mine = ++asked.current;
    const [c, f, x, t] = await Promise.all(
      [shelf.card(id), shelf.form(id), shelf.extras(id), shelf.tags()]);
    if (mine !== asked.current) return;
    if (c.ok) setCard(c.value);
    else setProblem(c.problem.messages);
    if (f.ok) setFields(f.value);
    else setProblem(f.problem.messages);
    if (x.ok) setExtras(x.value);
    if (t.ok) setKnown(t.value.map((k) => k.tag));
  }, [id]);
  useEffect(() => { reload(); }, [reload]);
  // ANCHOR_END: reload

  // ✅ Change through the API, then read the card again (or say why not).
  //    Says whether it worked, so a form keeps what was typed on a no.
  // API を通して変えて、カードを読み直す（だめなら理由を見せる）。
  // うまくいったかを返す（だめなら、フォームは打ったものを残す）
  const change = async (sent: Promise<Answer<unknown>>) => {
    const a = await sent;
    setProblem(a.ok ? [] : a.problem.messages);
    await reload();
    return a.ok;
  };

  // ✅ A tick writes today in the log, for the entry and every entry
  //    under it; taking it off drops their lines. One at a time, and
  //    the first refusal stops the rest.
  // チェックを入れたら、その項目と下の全部について、今日の日付でログに
  // 書く。外したら、その行を消す。1つずつ順に、断られたらそこで止める
  const mark = (entry: number, read: boolean) => {
    if (card === null) return;
    const today = dayOf(new Date());
    const asks = marksFor(card.toc, entry, read).map((e) => () =>
      read ? shelf.logReading(id, today, e) : shelf.unread(id, e));
    change(inOrder(asks));
  };

  if (card === null || extras === null) return problem.length > 0
    ? <Problems messages={problem} />
    : <Text>読みこみ中…</Text>;
  const w = card.work;
  return (
    <Stack>
      {/* ANCHOR: head */}
      <Group justify="space-between" align="flex-start" wrap="nowrap">
        <div>
          <Title order={1}>{w.title}</Title>
          <Text c="dimmed">{w.creator}</Text>
          <Group gap={6} mt={6}>
            {extras.tags.map((t) => (
              <Badge key={t} variant="outline" style={{ cursor: "pointer" }}
                     onClick={() => onTag(t)}>#{t}</Badge>))}
          </Group>
        </div>
        <Button variant="default" leftSection={<IconPencil size={16} />}
                onClick={onEdit}>直す</Button>
      </Group>
      <Problems messages={problem} />
      {/* ANCHOR_END: head */}
      {/* ANCHOR: reading */}
      <Paper withBorder p="md">
        <Group justify="space-between">
          <Group gap="xs">
            <Badge size="lg" color={stateColor(w.reading)}>
              {stateLabel(w.reading)}
            </Badge>
            {card.next.map((e) => (
              <Button key={e} size="xs" variant="light"
                      onClick={() => change(shelf.happen(id, e))}>
                {eventLabel(e)}
              </Button>))}
          </Group>
          <Rating value={w.rating === null ? 0 : halfStars(w.rating)}
                  fractions={2} size="lg" color={tone.star}
                  onChange={(v) => change(shelf.rate(id, pointsOf(v)))} />
        </Group>
        {w.progress && <Bar p={w.progress} />}
      </Paper>
      {/* ANCHOR_END: reading */}
      {/* ANCHOR: tabs */}
      <Grid>
        <Grid.Col span={{ base: 12, sm: 8 }}>
          <Tabs defaultValue={card.toc.length > 0 ? "toc" : "notes"}>
            <Tabs.List>
              {card.toc.length > 0 && <Tabs.Tab value="toc">目次</Tabs.Tab>}
              <Tabs.Tab value="notes">感想 {extras.seen.length}</Tabs.Tab>
              <Tabs.Tab value="attrs">属性</Tabs.Tab>
            </Tabs.List>
            <Tabs.Panel value="toc" pt="md">
              <Toc items={card.toc} onMark={mark} />
            </Tabs.Panel>
            <Tabs.Panel value="notes" pt="md">
              <NoteForm onWrite={(body, spoiler) =>
                change(shelf.addNote(id, body, spoiler))} />
              <Notes seen={extras.seen} toc={card.toc} />
            </Tabs.Panel>
            <Tabs.Panel value="attrs" pt="md">
              <AttrForm fields={fields} onSave={(edits) =>
                change(shelf.editAttrs(id, edits))} />
            </Tabs.Panel>
          </Tabs>
        </Grid.Col>
        <Grid.Col span={{ base: 12, sm: 4 }}>
          <Stack>
            <TagsBox tags={extras.tags} known={known}
                     onSave={(ts) => change(shelf.setTags(id, ts))} />
            <Alike extras={extras} onOpen={onOpen} />
          </Stack>
        </Grid.Col>
      </Grid>
      {/* ANCHOR_END: tabs */}
    </Stack>
  );
}
// ANCHOR_END: card

// ANCHOR: inOrder
// 🔁 Ask one after another; the first refusal stops the rest.
// 1つずつ順に聞く。断られたら、そこで止める
async function inOrder(
  asks: (() => Promise<Answer<unknown>>)[],
): Promise<Answer<unknown>> {
  let last: Answer<unknown> = { ok: true, value: null };
  for (const ask of asks) {
    last = await ask();
    if (!last.ok) break;
  }
  return last;
}
// ANCHOR_END: inOrder

// ANCHOR: notes
// 💬 The notes as I may see them now, newest first. A hidden note came
//    with its hint only: there is no body here to show by mistake. The
//    `switch` on `tag` is how TypeScript tells the two kinds apart.
// いまの自分に見せてよい感想を、新しい順に。隠した感想は、ヒントだけが
// 届いた。うっかり見せてしまう本文は、ここにはない。2つの種類を見分けるのは、
// tag での switch
function Notes({ seen, toc }: { seen: SeenNote[]; toc: TocItem[] }) {
  if (seen.length === 0) return <Text c="dimmed">まだ感想はありません</Text>;
  return (
    <Timeline bulletSize={26} lineWidth={2} mt="md">
      {[...seen].reverse().map((s, i) => {
        switch (s.tag) {
          case "ShownNote":
            return (
              <Timeline.Item key={i} bullet={<IconMessage size={14} />}
                title={whereOf(toc, s.note.entry)}>
                <Text size="xs" c="dimmed">
                  {s.note.writtenAt.slice(0, 10)}
                  {s.note.spoiler && " ・ネタバレ（もう読んだところ）"}
                </Text>
                <Text>{s.note.body}</Text>
              </Timeline.Item>);
          case "HiddenNote":
            return (
              <Timeline.Item key={i} bullet={<IconLock size={14} />}
                title={whereOf(toc, s.entry)} color="gray">
                <Text c="dimmed" fs="italic">{s.hint}</Text>
              </Timeline.Item>);
        }
      })}
    </Timeline>
  );
}
// ANCHOR_END: notes

// ANCHOR: tagsBox
// 🏷️ The box of tags: type, and press Enter (or a comma) to make a
//    pill; the tags already on the shelf are offered. Mantine trims the
//    blanks; whether a tag is good is the core's to say (too long, too
//    many), and its words come back as a problem.
// タグの箱：打って Enter（かカンマ）で札に。本棚にあるタグを候補に出す。
// 前後の空白は Mantine が落とす。タグがよいかどうか（長すぎ、多すぎ）は
// 核が言い、その言葉が problem で返ってくる
function TagsBox({ tags, known, onSave }: {
  tags: string[];
  known: string[];
  onSave: (tags: string[]) => Promise<boolean>;
}) {
  const [typed, setTyped] = useState<string[]>(tags);
  useEffect(() => { setTyped(tags); }, [tags]);
  return (
    <Paper withBorder p="md">
      <TagsInput label="タグ" leftSection={<IconTag size={16} />}
                 data={known} value={typed} onChange={setTyped}
                 splitChars={[",", "、"]} clearable />
      <Button mt="sm" size="xs" variant="light"
              onClick={() => onSave(typed)}>タグを保存</Button>
    </Paper>
  );
}
// ANCHOR_END: tagsBox

// ANCHOR: alike
// 🤝 The works most like this one, by their tags (the core's Jaccard,
//    in percent).
// タグがこの作品に似ている作品（核の Jaccard 係数を、百分率で）
function Alike({ extras, onOpen }: {
  extras: ExtrasItem;
  onOpen: (id: number) => void;
}) {
  return (
    <Paper withBorder p="md">
      <Text fw={700} mb="xs">この作品が好きなら</Text>
      {extras.alike.length === 0 &&
        <Text size="sm" c="dimmed">
          タグを付けると、似ている作品が出ます
        </Text>}
      <Stack gap="xs">
        {extras.alike.map((a) => (
          <Group key={a.work} justify="space-between" wrap="nowrap">
            <Button variant="subtle" size="compact-sm"
                    onClick={() => onOpen(a.work)}>{a.title}</Button>
            <Badge variant="light" color="gray">{a.percent}%</Badge>
          </Group>))}
      </Stack>
    </Paper>
  );
}
// ANCHOR_END: alike

// ANCHOR: noteForm
// ✍️ The form for a new note.
// 新しい感想のフォーム
function NoteForm({ onWrite }: {
  onWrite: (body: string, spoiler: boolean) => Promise<boolean>;
}) {
  const [body, setBody] = useState("");
  const [spoiler, setSpoiler] = useState(false);
  // Clear the form only when the note was written.
  // 書けたときだけ、フォームを空にする
  const write = async (e: React.FormEvent) => {
    e.preventDefault();
    if (await onWrite(body, spoiler)) {
      setBody("");
      setSpoiler(false);
    }
  };
  return (
    <form onSubmit={write}>
      <Textarea autosize minRows={2} value={body} placeholder="感想を書く"
                onChange={(e) => setBody(e.currentTarget.value)} />
      <Group justify="space-between" mt="xs">
        <Switch label="ネタバレ" checked={spoiler}
                onChange={(e) => setSpoiler(e.currentTarget.checked)} />
        <Button type="submit" size="xs">書く</Button>
      </Group>
    </form>
  );
}
// ANCHOR_END: noteForm
// ANCHOR_END: all
