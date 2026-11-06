%% The Erlang code in the "design" chapter of the message broker part.
%% Run with escript.
%% メッセージブローカーの部の「設計」の章の Erlang のコード。escript で動かす
-mode(compile).

%% ANCHOR: mailbox
%% Erlang (every process already has a mailbox: a queue of letters)
%% Erlang（どのプロセスも、はじめから郵便受け＝手紙のキューを持っている）

%% 📬 A team that reads its letters one by one, in order.
%% 手紙を1通ずつ、届いた順に読むチーム
team(Read) ->
    receive
        {letter, Body} -> team([Body | Read]);
        {report, From} -> From ! {read, lists:reverse(Read)}
    end.

morning() ->
    Team = spawn(fun() -> team([]) end),
    Team ! {letter, <<"A red dragon at the north gate">>},
    Team ! {letter, <<"A baby dragon is lost">>},
    Team ! {report, self()},
    receive
        {read, Letters} -> Letters
    end.
%% ANCHOR_END: mailbox

main(_) ->
    [<<"A red dragon at the north gate">>, <<"A baby dragon is lost">>] =
        morning(),
    io:format("ok~n").
