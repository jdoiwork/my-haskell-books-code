// The React search box in the search Web API chapter
// (type-checked only: it runs in a browser).
// 検索 Web API の章の React の検索ボックス
// （ブラウザで動くので、型だけを確かめる）
import { useState } from "react";
import { searchDocs, type HitView } from "./search-client.js";

// ANCHOR: box
// TypeScript (React: a search box that calls the Haskell API)
// TypeScript（React：Haskell の API を呼ぶ検索ボックス）
export function SearchBox() {
  const [q, setQ] = useState("");
  const [hits, setHits] = useState<HitView[]>([]);
  const [error, setError] = useState<string | null>(null);

  const onSearch = async () => {
    const res = await searchDocs("http://localhost:8080", q, 10);
    if ("message" in res) {
      setError(res.message);
      setHits([]);
    } else {
      setError(null);
      setHits(res.hits);
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
