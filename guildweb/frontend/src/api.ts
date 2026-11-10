// ANCHOR: all
// Talking to the Haskell server. The types come from Haskell (types.ts).
// Haskell のサーバーと話す。型は Haskell から作ったもの（types.ts）
import type { ApiError, NewQuest, Quest } from "./types";

// 📮 Send a request; on an error, throw the server's message.
// 頼みを送る。エラーなら、サーバーのメッセージを投げる
async function call<T>(path: string, init?: RequestInit): Promise<T> {
  const response = await fetch(path, {
    headers: { "Content-Type": "application/json" },
    ...init,
  });
  if (!response.ok) {
    throw new Error(messageOf(response.status, await response.text()));
  }
  return response.json();
}

// 💬 Our own errors are ApiError JSON. Others (a body servant could not
//    read, a proxy that is down) may be plain text: show them as they are.
// 自分で返すエラーは ApiError の JSON。ほか（servant が読めなかった本文、
// 止まった中継など）はただの文字のことがあるので、そのまま見せる
function messageOf(status: number, body: string): string {
  try {
    const error: ApiError = JSON.parse(body);
    return error.message;
  } catch {
    return `${status}: ${body}`;
  }
}

export const fetchQuests = () => call<Quest[]>("/api/quests");

export const postQuest = (quest: NewQuest) =>
  call<Quest>("/api/quests", { method: "POST", body: JSON.stringify(quest) });

export const finishQuest = (id: number) =>
  call<Quest>(`/api/quests/${id}/done`, { method: "POST" });
// ANCHOR_END: all
