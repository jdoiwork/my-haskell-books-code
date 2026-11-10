// The React search box in the search Web API chapter
// (type-checked only: it runs in a browser).
// 検索 Web API の章の React の検索ボックス
// （ブラウザで動くので、型だけを確かめる）
import { useRef, useState } from "react";
import { searchDocs, type HitView } from "./search-client.js";

// ANCHOR: box
// TypeScript (React: a search box that calls the Haskell API)
// TypeScript（React：Haskell の API を呼ぶ検索ボックス）
export function SearchBox() {
  const [q, setQ] = useState("");
  const [hits, setHits] = useState<HitView[]>([]);
  const [error, setError] = useState<string | null>(null);
  // The number of the latest search. An older answer that arrives
  // late must not overwrite a newer one.
  // いちばん新しい検索の番号。遅れて届いた古い答えで、
  // 新しい答えを上書きしない
  const latest = useRef(0);

  const onSearch = async () => {
    const mine = ++latest.current;
    try {
      // "": the same origin; the dev server passes /search on.
      // ""：同じ Origin。開発サーバーが /search を中継する
      const res = await searchDocs("", q, 10);
      if (mine !== latest.current) return;
      if ("message" in res) {
        setError(res.message);
        setHits([]);
      } else {
        setError(null);
        setHits(res.hits);
      }
    } catch (e) {
      // The server is down, or the answer is not JSON.
      // サーバーが止まっている、答えが JSON でない、など
      if (mine !== latest.current) return;
      setError((e as Error).message);
      setHits([]);
    }
  };

  return (
    <div>
      <input value={q} onChange={(e) => setQ(e.target.value)} />
      <button onClick={onSearch}>Search</button>
      {error && <p className="error">{error}</p>}
      <ol>
        {hits.map((h) => (
          <li key={h.hitId}>
            <b>{h.title}</b> {h.excerpt}
          </li>
        ))}
      </ol>
    </div>
  );
}
// ANCHOR_END: box
