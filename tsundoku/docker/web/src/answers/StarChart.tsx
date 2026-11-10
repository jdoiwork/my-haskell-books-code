// ANCHOR: all
// Training: the spread of the stars on the shelf, as Mantine's BarChart.
// The counting is the React chapter's histogram(), as it was.
// トレーニング：本棚の★の分布を、Mantine の BarChart で。数えるのは、
// React の章の histogram() のまま
import { BarChart } from "@mantine/charts";

import { stars } from "../shadows";
import { histogram } from "./Histogram";

// 📊 One row for each mark, lowest first: what the chart wants.
// 目盛りごとに1行、低い順：グラフが欲しい形
export const starRows = (points: (number | null)[]) =>
  [...histogram(points)].map(([mark, works]) =>
    ({ mark: stars(mark), works }));

export function StarChart({ points }: { points: (number | null)[] }) {
  return (
    <BarChart h={200} dataKey="mark" data={starRows(points)}
              series={[{ name: "works", label: "作品",
                         color: "yellow.6" }]} />
  );
}
// ANCHOR_END: all
