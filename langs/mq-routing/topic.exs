# The Elixir code in the "routing" chapter of the message broker part.
# Run with elixir.
# メッセージブローカーの部の「仕分け」の章の Elixir のコード。elixir で動かす
defmodule Topic do
  def matches?(pattern, key) do
    match(String.split(pattern, "."), String.split(key, "."))
  end

  # ANCHOR: match
  # Elixir (function heads, like Haskell's equations)
  # Elixir（関数の頭で場合分け。Haskell の等式と同じ形）

  # 🔀 Does a pattern of words match a key of words?
  # 語のパターンが、語のキーに合うか
  defp match([], []), do: true
  defp match(["#" | ps], ws), do: Enum.any?(tails(ws), &match(ps, &1))
  defp match(["*" | ps], [_ | ws]), do: match(ps, ws)
  # The same name twice: both words must be equal.
  # 同じ名前を2回書くと「2つの語が等しい」という意味になる
  defp match([w | ps], [w | ws]), do: match(ps, ws)
  defp match(_, _), do: false

  # All the endings of a list: [a, b] -> [[a, b], [b], []]
  # リストの後ろの部分を全部：[a, b] -> [[a, b], [b], []]
  defp tails([]), do: [[]]
  defp tails([_ | rest] = ws), do: [ws | tails(rest)]
  # ANCHOR_END: match
end

true = Topic.matches?("quest.*", "quest.dragon")
false = Topic.matches?("quest.*", "quest")
true = Topic.matches?("quest.#", "quest")
true = Topic.matches?("quest.#", "quest.dragon.north")
true = Topic.matches?("quest.*.north", "quest.herb.north")
false = Topic.matches?("quest.dragon.*", "quest.dragon.north.cave")
false = Topic.matches?("quest.dragon", "quest.herb")
true = Topic.matches?("#", "")
true = Topic.matches?("#.north", "quest.dragon.north")
IO.puts("ok")
