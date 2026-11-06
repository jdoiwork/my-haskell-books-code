%% The Erlang code in the "ack" chapter of the message broker part.
%% Run with escript.
%% メッセージブローカーの部の「受け取りの返事」の章の Erlang のコード。
%% escript で動かす
-mode(compile).

%% ANCHOR: monitor
%% Erlang (watch the taker; if it dies, the letter comes back)
%% Erlang（受け取った人を見張る。倒れたら、手紙は戻ってくる）

%% 📮 A mailbox: letters waiting, and letters out with someone.
%% 郵便受け：待っている手紙と、だれかが持ち出している手紙
mailbox(Waiting, Out) ->
    receive
        {take, From} when Waiting =/= [] ->
            [Letter | Rest] = Waiting,
            Ref = erlang:monitor(process, From),
            From ! {deliver, Ref, Letter},
            mailbox(Rest, Out#{Ref => Letter});
        {ack, Ref} ->
            erlang:demonitor(Ref, [flush]),
            mailbox(Waiting, maps:remove(Ref, Out));
        {'DOWN', Ref, process, _, _} ->
            % No ack: put the letter back at the front.
            % 返事がなかった。手紙を先頭に戻す
            #{Ref := Letter} = Out,
            mailbox([Letter | Waiting], maps:remove(Ref, Out));
        {peek, From} ->
            From ! {waiting, Waiting, maps:size(Out)},
            mailbox(Waiting, Out)
    end.
%% ANCHOR_END: monitor

take(Box) ->
    Box ! {take, self()},
    receive {deliver, Ref, Letter} -> {Ref, Letter} end.

peek(Box) ->
    Box ! {peek, self()},
    receive {waiting, Waiting, Out} -> {Waiting, Out} end.

%% The box gets its own 'DOWN' a moment later: wait for it.
%% 郵便受けにも、少しあとで 'DOWN' が届く。それを待つ
wait_until_back(Box) ->
    case peek(Box) of
        {[_, _], 0} -> ok;
        _ -> timer:sleep(1), wait_until_back(Box)
    end.

main(_) ->
    Box = spawn(fun() ->
                    mailbox([<<"Slay the dragon">>, <<"Pick herbs">>], #{})
                end),
    Me = self(),
    %% Consumer A takes a letter and falls without an ack.
    %% 受け手 A は手紙を受け取り、返事をせずに倒れる
    {A, AMon} = spawn_monitor(fun() ->
                                  {_, Letter} = take(Box),
                                  Me ! {a_took, Letter},
                                  exit(boom)
                              end),
    <<"Slay the dragon">> = receive {a_took, L} -> L end,
    receive {'DOWN', AMon, process, A, boom} -> ok end,
    wait_until_back(Box),
    %% The letter is back at the front; B takes it and acks.
    %% 手紙は先頭に戻っている。B が受け取って、返事をする
    {Ref, <<"Slay the dragon">>} = take(Box),
    {[<<"Pick herbs">>], 1} = peek(Box),
    Box ! {ack, Ref},
    {[<<"Pick herbs">>], 0} = peek(Box),
    {Ref2, <<"Pick herbs">>} = take(Box),
    Box ! {ack, Ref2},
    {[], 0} = peek(Box),
    io:format("ok~n").
