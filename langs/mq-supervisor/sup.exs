# The Elixir code in the "supervisor" chapter of the message broker part.
# Run with elixir.
# メッセージブローカーの部の「監督役」の章の Elixir のコード。
# elixir で動かす

# The crash reports are expected here: keep them quiet.
# ここでは倒れるのが予定どおりなので、報告は出さない
Logger.configure(level: :none)

# ANCHOR: supervisor
# Elixir (the same OTP supervisor, with an Agent counting the starts)
# Elixir（同じ OTP の supervisor。起きた回数は Agent で数える）
defmodule Worker do
  use GenServer

  def start_link(starts), do: GenServer.start_link(__MODULE__, starts)

  # 👷 Fall the first two times, then keep going.
  # はじめの2回は倒れ、3回目からは働き続ける
  @impl true
  def init(starts) do
    count = Agent.get_and_update(starts, fn n -> {n + 1, n + 1} end)
    if count <= 2, do: Process.send_after(self(), :fall, 10)
    {:ok, count}
  end

  @impl true
  def handle_info(:fall, count), do: {:stop, :crash, count}
end

{:ok, starts} = Agent.start_link(fn -> 0 end)

{:ok, sup} =
  Supervisor.start_link([{Worker, starts}],
    strategy: :one_for_one,
    max_restarts: 3,
    max_seconds: 10
  )
# ANCHOR_END: supervisor

defmodule Wait do
  def starts(agent, n) do
    if Agent.get(agent, & &1) == n do
      :ok
    else
      Process.sleep(5)
      starts(agent, n)
    end
  end
end

:ok = Wait.starts(starts, 3)
Process.sleep(50)
3 = Agent.get(starts, & &1)
true = Process.alive?(sup)
[{Worker, pid, :worker, _}] = Supervisor.which_children(sup)
true = Process.alive?(pid)
IO.puts("ok")
