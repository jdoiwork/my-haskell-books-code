%% The Erlang code in the "typed client" chapter of the broker part.
%% Run with escript.
%% メッセージブローカーの部の「型に厳しいクライアント」の章の Erlang のコード。
%% escript で動かす
-module(consume).
-export([main/1]).

%% ANCHOR: consume
%% Erlang (the answer comes from how the handler ends)
%% Erlang（返事は、受け手の関数がどう終わったかで決まる）

%% 📨 One answer for each letter: ack, requeue or reject.
%%    Let the handler fail; `catch` turns the failure into the answer.
%% 手紙1通ごとに、返事を1つ：ack、requeue、reject。
%% 受け手の関数は失敗してよい。catch が、失敗を返事に変える
answer(Body, Handle) ->
    case read(Body) of
        {ok, Quest} ->
            try Handle(Quest) of
                _ -> ack
            catch
                _:_ -> requeue
            end;
        error ->
            reject
    end.

%% 📖 "title;reward" -> {ok, {Title, Reward}}, or error
%% 「題名;報酬」を依頼にする。読めなければ error
read(Body) ->
    case string:split(Body, ";") of
        [Title, Reward] ->
            try {ok, {Title, binary_to_integer(Reward)}}
            catch error:badarg -> error
            end;
        _ ->
            error
    end.
%% ANCHOR_END: consume

main(_) ->
    Handle = fun({_Title, Gold}) when Gold > 0 -> ok;
                ({_Title, _}) -> error(no_reward_yet)
             end,
    Answers = [answer(B, Handle) || B <- [<<"Slay the dragon;500">>,
                                         <<"Find the cat;0">>,
                                         <<"<<smudged>>">>,
                                         <<"Pick herbs;many">>]],
    [ack, requeue, reject, reject] = Answers,
    io:format("ok~n").
