// ANCHOR: all
// Training: the spread of the stars on the shelf, as bars.
// トレーニング：本棚の★の分布を、横棒で
import { marks, stars } from "../shadows";

// 📊 How many works sit on each mark (★0 .. ★5, every half star).
//    No rating: not counted. The middle rounds up, as stars() does.
// 目盛り（★0〜★5、半分刻み）ごとに、何作品あるか。評価のない作品は
// 数えない。真ん中は切り上げ（stars と同じ）
export function histogram(points: (number | null)[]): Map<number, number> {
  const counts = new Map<number, number>(marks.map((m) => [m, 0]));
  for (const p of points) {
    if (p === null) continue;
    const mark = Math.floor((p + 5) / 10) * 10;
    counts.set(mark, (counts.get(mark) ?? 0) + 1);
  }
  return counts;
}

// 📊 The bars, highest mark first.
// 横棒。高い目盛りから
export function Histogram({ points }: { points: (number | null)[] }) {
  const counts = [...histogram(points)].reverse();
  return (
    <table className="histogram">
      <tbody>
        {counts.map(([mark, n]) => (
          <tr key={mark}>
            <th>{stars(mark)}</th>
            <td><span className="bar" style={{ width: `${n}rem` }} /> {n}</td>
          </tr>))}
      </tbody>
    </table>
  );
}
// ANCHOR_END: all
