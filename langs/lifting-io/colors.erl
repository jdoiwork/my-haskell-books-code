%% The Erlang code in the "lifting IO" chapter. Run with escript.
%% IO を持ち上げるの章の Erlang のコード。escript で動かす
-mode(compile).

%% ANCHOR: nocolor
%% Erlang (no colors: any plain function can wait with receive)
%% Erlang（色がない：どのふつうの関数も、receive で待てる）
appraise(Item) ->
    timer:sleep(100),
    length(Item) * 100.

appraise_all(Items) ->
    Me = self(),
    %% The caller decides: one process for each item.
    %% 同時に動かすかは、呼ぶ側が決める：品物ごとにプロセスを1つ
    Pids = [spawn(fun() -> Me ! {self(), appraise(Item)} end)
            || Item <- Items],
    %% Wait for the answers, in the order of the items.
    %% 品物の順に、答えのメッセージを待つ
    [receive {Pid, Price} -> Price end || Pid <- Pids].

closing_report(Items) ->
    lists:sum(appraise_all(Items)).
%% ANCHOR_END: nocolor

main(_) ->
    Start = erlang:monotonic_time(millisecond),
    1500 = closing_report(["sword", "shield", "herb"]),
    true = erlang:monotonic_time(millisecond) - Start < 250,
    [400] = appraise_all(["herb"]),
    io:format("ok~n").
