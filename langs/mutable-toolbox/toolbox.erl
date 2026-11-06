%% The Erlang code in the "choosing a box" chapter. Run with escript.
%% 書き換えられる箱の使い分けの章の Erlang のコード。escript で動かす
-mode(compile).

%% ANCHOR: process
%% Erlang (the ticket machine is a process: only it holds the number)
%% Erlang（番号札の機械は1つのプロセス。番号を持っているのは、そのプロセスだけ）
start_ticket_machine() ->
    spawn(fun() -> machine(1) end).

machine(Next) ->
    receive
        {take, From} ->
            From ! {ticket, Next},
            machine(Next + 1)
    end.

take_ticket(Machine) ->
    Machine ! {take, self()},
    receive
        {ticket, N} -> N
    end.
%% ANCHOR_END: process

%% ANCHOR: visitors
%% Erlang (100 visitors at once, 10 tickets each)
%% Erlang（100人が同時に10枚ずつ）
ticket_rush() ->
    Machine = start_ticket_machine(),
    Me = self(),
    [spawn(fun() ->
               Mine = [take_ticket(Machine) || _ <- lists:seq(1, 10)],
               Me ! {done, Mine}
           end)
     || _ <- lists:seq(1, 100)],
    All = lists:append([receive {done, Mine} -> Mine end
                        || _ <- lists:seq(1, 100)]),
    lists:sort(All) =:= lists:seq(1, 1000).
%% ANCHOR_END: visitors

main(_) ->
    M = start_ticket_machine(),
    [1, 2, 3] = [take_ticket(M), take_ticket(M), take_ticket(M)],
    true = ticket_rush(),
    io:format("ok~n").
