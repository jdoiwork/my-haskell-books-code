# Elixir: a comprehension that skips what does not fit, the pipe |>
# that passes the value as the FIRST argument, and & for short functions.
# Elixir：合わないものを飛ばす内包表記、値を「最初の」引数に渡す |>、
# 短い関数を作る &
scores = [{"Python", 82}, {"TS", 75}, {"Rust", 91}, {"Go", 68}]

# ANCHOR: elixir
# Elixir
passed = for {name, score} <- scores, score >= 70, do: name

total =
  scores
  |> Enum.map(fn {_, score} -> score end)
  |> Enum.sum()

bonus = &min(&1 + 5, 100)

# A value that does not fit {:ok, x} is skipped.
# {:ok, x} に合わない値は、飛ばされる
oks = for {:ok, x} <- [{:ok, 1}, :error, {:ok, 3}], do: x
# ANCHOR_END: elixir

if passed != ["Python", "TS", "Rust"], do: raise("passed")
if total != 316, do: raise("total")
if bonus.(98) != 100, do: raise("bonus")
if oks != [1, 3], do: raise("oks")
