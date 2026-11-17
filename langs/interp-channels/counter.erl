%% Erlang: the counter as a process, ask with a reply address, pick the
%% message you want (selective receive), give up with after, and watch a
%% process with a monitor. The same as counter.lam, selective.lam, and
%% monitor.lam. Run with escript.
%% Erlang：プロセスのカウンター、返事の宛先を付けて頼む、欲しいメッセージを
%% 選んで取る、after であきらめる、モニターで見張る。counter.lam・
%% selective.lam・monitor.lam と同じ。escript で動かす
-module(counter).
-mode(compile).
-export([main/1]).

%% ANCHOR: counter
%% Erlang
%% 🧮 Only this process holds the total. self() is its own address.
%% 合計を持つのは、このプロセスだけ。self() は自分の宛先
counter(Total) ->
    receive
        {add, N}    -> counter(Total + N);
        {get, From} -> From ! {total, Total},
                       counter(Total)
    end.

%% 🙋 Ask and wait: our own address goes in the message.
%% 頼んで待つ：自分の宛先をメッセージに入れる
call_get(Counter) ->
    Counter ! {get, self()},
    receive {total, T} -> T end.
%% ANCHOR_END: counter

%% ANCHOR: selective
%% Erlang
%% 🍽️ Take dish 1 first, even if dish 2 came first; then give up on 3.
%% 2番が先に来ても、まず1番を取る。3番は、あきらめる
cook(To, N, Ms, Name) ->
    spawn(fun() -> timer:sleep(Ms), To ! {dish, N, Name} end).

dinner() ->
    Me = self(),
    cook(Me, 1, 30, "curry"),
    cook(Me, 2, 10, "salad"),
    cook(Me, 3, 900, "cake"),
    First = receive {dish, 1, A} -> A end,
    Then  = receive {dish, _, B} -> B end,
    Last  = receive {dish, 3, C} -> C
            after 50 -> "no cake yet"
            end,
    {First, Then, Last}.
%% ANCHOR_END: selective

%% ANCHOR: monitor
%% Erlang
%% 👀 A process that crashes, and a monitor that tells us how.
%% 落ちるプロセスと、どう終わったかを教えてくれるモニター
average(Xs) -> lists:sum(Xs) div length(Xs).

watch() ->
    {Pid, Ref} = spawn_monitor(fun() -> average([]) end),
    receive {'DOWN', Ref, process, Pid, Why} -> Why end.
%% ANCHOR_END: monitor

main(_) ->
    C = spawn(fun() -> counter(0) end),
    [C ! {add, N} || N <- [3, 4, 5]],
    12 = call_get(C),
    {"curry", "salad", "no cake yet"} = dinner(),
    {badarith, _} = watch(),
    io:format("ok~n").
