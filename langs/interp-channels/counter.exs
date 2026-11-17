# Elixir: the counter as a process (spawn, send, receive), a reply with a
# pin (^ref), selective receive with after, and a Supervisor that starts a
# fallen Agent again. The same as counter.lam and supervise.lam.
# Elixir：プロセスのカウンター（spawn・send・receive）、^ref で返事を選ぶ、
# after つきの選んで受け取る形、倒れた Agent を起こし直す Supervisor。
# counter.lam・supervise.lam と同じ

# ANCHOR: counter
# Elixir
defmodule Counter do
  # 🧮 Only this process holds the total.
  # 合計を持つのは、このプロセスだけ
  def loop(total) do
    receive do
      {:add, n} -> loop(total + n)
      {:get, from, ref} ->
        send(from, {ref, total})
        loop(total)
    end
  end

  # 🙋 Ask, and take only the answer with our ref.
  # 頼んで、自分の ref の付いた答えだけを取る
  def get(pid) do
    ref = make_ref()
    send(pid, {:get, self(), ref})
    receive do
      {^ref, total} -> total
    after
      500 -> :no_answer
    end
  end
end
# ANCHOR_END: counter

# ANCHOR: supervisor
# Elixir
# 🛟 An Agent that holds a list, under a Supervisor. If the Agent
#    crashes, the Supervisor starts it again, under the same name.
# リストを持つ Agent を、Supervisor の下に。Agent が落ちたら、Supervisor
# が同じ名前で起こし直す
start = {Agent, :start_link, [fn -> [] end, [name: :scores]]}
children = [%{id: :scores, start: start}]
{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

Agent.update(:scores, fn xs -> [3 | xs] end)
old = Process.whereis(:scores)
Process.exit(old, :kill)
Process.sleep(50)
# A new process, the same name, and a new (empty) state.
# 新しいプロセス、同じ名前、新しい（空の）状態
new = Process.whereis(:scores)
# ANCHOR_END: supervisor

true = is_pid(new) and new != old
[] = Agent.get(:scores, fn xs -> xs end)

c = spawn(fn -> Counter.loop(0) end)
for n <- [3, 4, 5], do: send(c, {:add, n})
12 = Counter.get(c)
IO.puts("ok")
