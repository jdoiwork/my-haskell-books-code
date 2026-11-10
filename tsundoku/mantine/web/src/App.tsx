// ANCHOR: all
// The screen: the shelf (all of it, or one tag), one card, the form for
// a new work, the screen to fix a work, or the statistics. Which one is
// the only state here. Around it, Mantine's AppShell: the band at the
// top, with the way to each screen and the switch of light and dark.
// 画面：本棚（全部か、タグ1つ）、カード1枚、新しい作品のフォーム、作品を
// 直す画面、統計。ここにある状態は、どれを見せるかだけ。そのまわりに
// Mantine の AppShell：上の帯に、各画面への道と、明るい・暗いの切り替え
// ANCHOR: imports
import {
  ActionIcon, AppShell, Button, Container, Group, Loader, Title,
  UnstyledButton,
  useComputedColorScheme, useMantineColorScheme,
} from "@mantine/core";
import {
  IconChartBar, IconMoon, IconPlus, IconSun,
} from "@tabler/icons-react";
import { lazy, Suspense, useState } from "react";

import { CardView } from "./CardView";
import { EditWork } from "./EditWork";
import { NewWork } from "./NewWork";
import { WorkList } from "./WorkList";
// ANCHOR_END: imports

// ANCHOR: lazy
// 📦 The statistics page carries the charts (recharts), more than half
//    of the JavaScript. Fetch it only when it is opened.
// 統計のページは、グラフ（recharts）を抱えている。JavaScript の半分以上。
// 開いたときにだけ取りに行く
const Stats = lazy(() =>
  import("./Stats").then((m) => ({ default: m.Stats })));
// ANCHOR_END: lazy

// ANCHOR: screens
type Screen =
  | { on: "shelf"; tag: string | null }
  | { on: "card"; id: number }
  | { on: "new" }
  | { on: "edit"; id: number }
  | { on: "stats" };

export function App() {
  const [screen, setScreen] = useState<Screen>({ on: "shelf", tag: null });
  const open = (id: number) => setScreen({ on: "card", id });
  const shelfOf = (tag: string | null) => setScreen({ on: "shelf", tag });
  return (
    <AppShell header={{ height: 60 }} padding="md">
      <AppShell.Header>
        <Group h="100%" px="md" justify="space-between">
          <UnstyledButton onClick={() => shelfOf(null)}>
            <Title order={3}>📚 tsundoku</Title>
          </UnstyledButton>
          <Group gap="xs">
            <Button variant="subtle" leftSection={<IconChartBar size={18} />}
                    onClick={() => setScreen({ on: "stats" })}>統計</Button>
            <Button leftSection={<IconPlus size={18} />}
                    onClick={() => setScreen({ on: "new" })}>本を足す</Button>
            <LightDark />
          </Group>
        </Group>
      </AppShell.Header>
      <AppShell.Main>
        <Container size="md">
          {screen.on === "shelf" &&
            <WorkList tag={screen.tag} onTag={shelfOf} onOpen={open} />}
          {/* A new key for each work: opening a work alike starts a fresh
              card, with no state (or a note half typed) from the last. */}
          {/* 作品ごとに新しい key：似ている作品を開くと、前の作品の状態
              （打ちかけの感想も）を持たない、新しいカードから始まる */}
          {screen.on === "card" &&
            <CardView key={screen.id} id={screen.id}
              onTag={shelfOf} onOpen={open}
              onEdit={() => setScreen({ on: "edit", id: screen.id })} />}
          {screen.on === "new" && <NewWork onAdded={open} />}
          {screen.on === "edit" &&
            <EditWork id={screen.id} onDone={() => open(screen.id)} />}
          {screen.on === "stats" &&
            <Suspense fallback={<Loader />}>
              <Stats onTag={shelfOf} onOpen={open} />
            </Suspense>}
        </Container>
      </AppShell.Main>
    </AppShell>
  );
}
// ANCHOR_END: screens

// ANCHOR: lightDark
// 🌗 Light or dark. "auto" (the main.tsx default) follows the computer;
//    a click chooses one, and Mantine remembers it in localStorage.
// 明るいか、暗いか。"auto"（main.tsx の既定）はコンピューターに合わせる。
// 押すとどちらかに決まり、Mantine が localStorage に覚えておく
function LightDark() {
  const { setColorScheme } = useMantineColorScheme();
  const now = useComputedColorScheme("light");
  const other = now === "dark" ? "light" : "dark";
  return (
    <ActionIcon variant="default" size="lg" aria-label="明るい・暗い"
                onClick={() => setColorScheme(other)}>
      {now === "dark" ? <IconSun size={18} /> : <IconMoon size={18} />}
    </ActionIcon>
  );
}
// ANCHOR_END: lightDark
// ANCHOR_END: all
