# The Elixir code in the "typed client" chapter of the broker part.
# Run with elixir.
# メッセージブローカーの部の「型のあるクライアント」の章の Elixir のコード。
# elixir で動かす
defmodule Consume do
  # ANCHOR: consume
  # Elixir (the answer comes from how the handler ends)
  # Elixir（返事は、受け手の関数がどう終わったかで決まる）

  # 📖 "title;reward" -> a quest, or :error
  # 「題名;報酬」を依頼にする。読めなければ :error
  def decode(body) do
    with [title, reward] <- String.split(body, ";"),
         {gold, ""} <- Integer.parse(reward) do
      {:ok, %{title: title, reward: gold}}
    else
      _ -> :error
    end
  end

  # 📨 One answer for each letter: :ack, :requeue or :reject.
  # 手紙1通ごとに、返事を1つ：:ack、:requeue、:reject
  def answer(body, handle) do
    with {:ok, quest} <- decode(body) do
      case handle.(quest) do
        :ok -> :ack
        {:error, _} -> :requeue
      end
    else
      :error -> :reject
    end
  end

  def consume(bodies, handle) do
    Enum.map(bodies, &answer(&1, handle))
  end
  # ANCHOR_END: consume
end

handle = fn quest ->
  if quest.reward > 0, do: :ok, else: {:error, "no reward yet"}
end

answers =
  Consume.consume(
    ["Slay the dragon;500", "Find the cat;0", "<<smudged>>"],
    handle
  )

[:ack, :requeue, :reject] = answers
:error = Consume.decode("Pick herbs;many")
IO.puts("ok")
