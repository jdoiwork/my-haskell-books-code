// The TypeScript client in the search Web API chapter (for the React side).
// 検索 Web API の章の TypeScript のクライアント（React 側で使う）

// ANCHOR: types
// TypeScript (the same shapes as HitView and SearchResponse)
// TypeScript（HitView・SearchResponse と同じ形）
export type HitView = {
  hitId: number;
  title: string;
  score: number;
  excerpt: string;
};

export type SearchResponse = {
  query: string;
  total: number;
  hits: HitView[];
};

export type ApiError = { message: string };
// ANCHOR_END: types

// ANCHOR: url
// TypeScript (build the URL: URLSearchParams escapes the words)
// TypeScript（URL を作る：URLSearchParams が単語をエスケープする）
export function searchUrl(base: string, q: string, limit?: number): string {
  const params = new URLSearchParams({ q });
  if (limit !== undefined) {
    params.set("limit", String(limit));
  }
  return `${base}/search?${params.toString()}`;
}

export async function searchDocs(
  base: string,
  q: string,
  limit?: number,
): Promise<SearchResponse | ApiError> {
  const res = await fetch(searchUrl(base, q, limit));
  return (await res.json()) as SearchResponse | ApiError;
}
// ANCHOR_END: url

function check(ok: boolean, what: string): void {
  if (!ok) {
    throw new Error(`failed: ${what}`);
  }
}

const base = "http://localhost:8080";
check(
  searchUrl(base, "dragon fire", 5)
    === "http://localhost:8080/search?q=dragon+fire&limit=5",
  "spaces become +",
);
check(
  searchUrl(base, "gold&limit=1000")
    === "http://localhost:8080/search?q=gold%26limit%3D1000",
  "& cannot sneak in another parameter",
);
console.log("ok");
