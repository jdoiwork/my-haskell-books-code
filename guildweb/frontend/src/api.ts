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
    const error: ApiError = await response.json();
    throw new Error(error.message);
  }
  return response.json();
}

export const fetchQuests = () => call<Quest[]>("/api/quests");

export const postQuest = (quest: NewQuest) =>
  call<Quest>("/api/quests", { method: "POST", body: JSON.stringify(quest) });

export const finishQuest = (id: number) =>
  call<Quest>(`/api/quests/${id}/done`, { method: "POST" });
// ANCHOR_END: all
