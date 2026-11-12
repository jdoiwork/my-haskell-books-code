// ANCHOR: all
// The statistics as the charts want them. The API sends plain numbers
// (MonthItem, KindCount, TagCount); how to draw them is decided here,
// with no React, so each function is tested by itself (charts.test.ts).
// 統計を、グラフが欲しい形に。API は素朴な数（MonthItem・KindCount・
// TagCount）を送る。どう描くかは、ここで決める。React はないので、関数を
// 1つずつテストできる（charts.test.ts）
// ANCHOR: imports
import { kinds } from "./kinds";
import type { KindCount, MonthItem, TagCount } from "./tsundoku-api";
// ANCHOR_END: imports

// ANCHOR: months
// 📅 The twelve months of a year, each with its numbers; a month the
//    API did not send (nothing read) is 0, so the bars keep their places.
// 1年の12か月を、それぞれの数と一緒に。API が送らなかった月（何も読んで
// いない月）は 0。棒の位置がずれないように
export type MonthRow = { month: string; pages: number; finished: number };

export function monthRows(year: number, months: MonthItem[]): MonthRow[] {
  return Array.from({ length: 12 }, (_, i) => {
    const key = `${year}-${String(i + 1).padStart(2, "0")}`;
    const found = months.find((m) => m.month === key);
    return {
      month: `${i + 1}月`,
      pages: found?.pages ?? 0,
      finished: found?.finished ?? 0,
    };
  });
}
// ANCHOR_END: months

// ANCHOR: kinds
// 🍰 The kinds as slices of a donut: the screen's word, and a colour.
// 種類を、ドーナツのひと切れに：画面の言葉と、色
export type Slice = { name: string; value: number; color: string };

const kindColors: Record<string, string> = {
  book: "indigo.6", serial: "teal.6", series: "grape.6",
  film: "orange.6", game: "cyan.6",
};

export function kindSlices(counts: KindCount[]): Slice[] {
  return counts.map((c) => ({
    name: kinds.find((k) => k.word === c.kind)?.label ?? c.kind,
    value: c.works,
    color: kindColors[c.kind] ?? "gray.6",
  }));
}
// ANCHOR_END: kinds

// ANCHOR: cloud
// ☁️ The size of a tag in the cloud: the more works, the bigger, in
//    five steps from xs to xl. The most worn tag is always xl.
// 雲の中のタグの大きさ：作品が多いほど大きく、xs から xl の5段。
// いちばん多いタグは、いつも xl
const sizes = ["xs", "sm", "md", "lg", "xl"] as const;
export type CloudSize = (typeof sizes)[number];

export function cloudSize(works: number, cloud: TagCount[]): CloudSize {
  const most = Math.max(1, ...cloud.map((t) => t.works));
  const step = Math.ceil((works / most) * sizes.length) - 1;
  return sizes[Math.min(sizes.length - 1, Math.max(0, step))];
}
// ANCHOR_END: cloud

// ANCHOR: pace
// 🏁 The words for a pace: the day comes as "2026-11-14" (the core
//    counted it), or null when nothing was read in the last 7 days.
// ペースの言葉：日は "2026-11-14"（数えたのは核）。最近7日に読んでいな
// ければ null
export function paceWords(left: string, finishOn: string | null): string {
  if (finishOn === null) return `あと${left}。最近7日は読んでいません`;
  const [, month, day] = finishOn.split("-").map(Number);
  return `あと${left}。このペースなら、${month}月${day}日に読み終わります`;
}
// ANCHOR_END: pace
// ANCHOR_END: all
