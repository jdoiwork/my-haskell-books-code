// ANCHOR: all
// The shelf: one row for each work, and a search box.
// 本棚：作品ごとに1行と、探す箱
import { useEffect, useState } from "react";

import { shelf } from "./api";
import { Bar } from "./Bar";
import { stars, stateLabel } from "./shadows";
import type { HitItem, WorkSummary } from "./tsundoku-api";

// ANCHOR: list
export function WorkList({ onOpen }: { onOpen: (id: number) => void }) {
  const [works, setWorks] = useState<WorkSummary[]>([]);
  useEffect(() => {
    shelf.listWorks().then((a) => { if (a.ok) setWorks(a.value); });
  }, []);
  return (
    <>
      <Search onOpen={onOpen} />
      <ul className="works">
        {works.map((w) => (
          <li key={w.id}>
            <a href="#" onClick={() => onOpen(w.id)}>{w.title}</a>
            <span className="creator">{w.creator}</span>
            <span className="state">{stateLabel(w.reading)}</span>
            <span className="rating">{stars(w.rating)}</span>
            {w.progress && <Bar p={w.progress} />}
          </li>
        ))}
      </ul>
    </>
  );
}
// ANCHOR_END: list

// ANCHOR: search
// 🔎 Search the shelf. The matched pieces are <mark>ed here, by the
//    screen; the API only said which pieces matched.
// 本棚を探す。当たったかけらに <mark> を付けるのは、ここ（画面）。
// API は、どのかけらが当たったかを言っただけ
function Search({ onOpen }: { onOpen: (id: number) => void }) {
  const [q, setQ] = useState("");
  const [hits, setHits] = useState<HitItem[]>([]);
  const [why, setWhy] = useState("");
  const find = async (e: React.FormEvent) => {
    e.preventDefault();
    const a = await shelf.search(q);
    if (a.ok) { setHits(a.value); setWhy(""); }
    else { setHits([]); setWhy(a.problem.messages.join(" ")); }
  };
  return (
    <form onSubmit={find}>
      <input type="search" value={q} placeholder="本棚の中を探す"
             onChange={(e) => setQ(e.target.value)} />
      {why && <p className="problem">{why}</p>}
      <ol className="hits">
        {hits.map((h, i) => (
          <li key={i}>
            <a href="#" onClick={() => onOpen(h.work)}>{h.title}</a>
            <p>{h.snippet.map((s, j) =>
              s.match ? <mark key={j}>{s.text}</mark> : s.text)}</p>
          </li>
        ))}
      </ol>
    </form>
  );
}
// ANCHOR_END: search
// ANCHOR_END: all
