// ANCHOR: all
// The shelf: one row for each work, and a search box.
// 本棚：作品ごとに1行と、探す箱
// ANCHOR: imports
import { useEffect, useRef, useState } from "react";

import { shelf } from "./api";
import { Bar } from "./Bar";
import { stars, stateLabel } from "./shadows";
import type { HitItem, WorkSummary } from "./tsundoku-api";
// ANCHOR_END: imports

// ANCHOR: list
export function WorkList({ onOpen }: { onOpen: (id: number) => void }) {
  const [works, setWorks] = useState<WorkSummary[]>([]);
  // why the shelf could not be read (shelf serve not running, say):
  // an empty list alone would look like an empty shelf
  // 本棚が読めなかった理由（shelf serve が動いていない、など）。
  // 空のリストだけだと、空の本棚に見えてしまう
  const [why, setWhy] = useState<string[]>([]);
  useEffect(() => {
    shelf.listWorks().then((a) => {
      if (a.ok) { setWorks(a.value); setWhy([]); }
      else setWhy(a.problem.messages);
    });
  }, []);
  return (
    <>
      <Search onOpen={onOpen} />
      {why.length > 0 && <ul className="problem">
        {why.map((m) => <li key={m}>{m}</li>)}</ul>}
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
