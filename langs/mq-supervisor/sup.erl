%% The Erlang code in the "supervisor" chapter of the message broker part.
%% Run with escript.
%% メッセージブローカーの部の「監督役」の章の Erlang のコード。
%% escript で動かす
-module(sup).
-behaviour(supervisor).
-mode(compile).
-export([main/1, init/1, start_worker/0, start_hopeless/0]).

%% ANCHOR: supervisor
%% Erlang (OTP's supervisor: restart a fallen worker, up to a limit)
%% Erlang（OTP の supervisor：倒れた働き手を、決まった回数まで起こす）

%% 👷 A worker that falls the first two times, then keeps going.
%% はじめの2回は倒れ、3回目からは働き続ける働き手
start_worker() ->
    Starts = ets:update_counter(starts, worker, 1),
    Pid = spawn_link(fun() ->
                         timer:sleep(10),
                         if Starts =< 2 -> exit(crash);
                            true        -> work()
                         end
                     end),
    {ok, Pid}.

work() ->
    receive stop -> ok end.

%% 🛡️ One worker; at most 3 restarts in 10 seconds.
%% 働き手は1人。10秒のうちに起こし直すのは3回まで
init(Start) ->
    Flags = #{strategy => one_for_one, intensity => 3, period => 10},
    Child = #{id => worker, start => {?MODULE, Start, []}},
    {ok, {Flags, [Child]}}.
%% ANCHOR_END: supervisor

%% ANCHOR: giveup
%% Erlang (a worker that always falls: the supervisor gives up too)
%% Erlang（いつも倒れる働き手：監督役も、あきらめて止まる）
start_hopeless() ->
    {ok, spawn_link(fun() -> exit(crash) end)}.

give_up() ->
    {ok, Sup} = supervisor:start_link(?MODULE, start_hopeless),
    unlink(Sup),
    Ref = erlang:monitor(process, Sup),
    receive
        {'DOWN', Ref, process, Sup, Reason} -> Reason
    end.
%% ANCHOR_END: giveup

wait_for_starts(N) ->
    case ets:lookup(starts, worker) of
        [{worker, N}] -> ok;
        _ -> timer:sleep(5), wait_for_starts(N)
    end.

main(_) ->
    %% The crash reports are expected here: keep them quiet.
    %% ここでは倒れるのが予定どおりなので、報告は出さない
    logger:set_primary_config(level, none),
    ets:new(starts, [named_table, public]),
    ets:insert(starts, {worker, 0}),
    {ok, Sup} = supervisor:start_link(?MODULE, start_worker),
    wait_for_starts(3),
    timer:sleep(50),
    [{worker, 3}] = ets:lookup(starts, worker),
    true = is_process_alive(Sup),
    [{worker, Pid, worker, _}] = supervisor:which_children(Sup),
    true = is_process_alive(Pid),
    shutdown = give_up(),
    io:format("ok~n").
