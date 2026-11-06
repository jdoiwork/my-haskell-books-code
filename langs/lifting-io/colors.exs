# The Elixir code in the "lifting IO" chapter. Run with elixir.
# IO を持ち上げるの章の Elixir のコード。elixir で動かす
defmodule Office do
  # ANCHOR: task
  # Elixir (Task.async is a plain function: no color, despite its name)
  # Elixir（Task.async は、ふつうの関数。名前は async でも、色ではない）
  def appraise(item) do
    Process.sleep(100)
    String.length(item) * 100
  end

  def appraise_all(items) do
    items
    |> Enum.map(fn item -> Task.async(fn -> appraise(item) end) end)
    |> Enum.map(&Task.await/1)
  end

  def closing_report(items) do
    items
    |> appraise_all()
    |> Enum.sum()
  end
  # ANCHOR_END: task
end

start = System.monotonic_time(:millisecond)
1500 = Office.closing_report(["sword", "shield", "herb"])
true = System.monotonic_time(:millisecond) - start < 250
[400] = Office.appraise_all(["herb"])
IO.puts("ok")
