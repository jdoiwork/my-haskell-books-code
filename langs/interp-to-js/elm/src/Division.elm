module Division exposing (main)

-- Elm's Int in JavaScript, for the chapter "Turning Lam into
-- JavaScript". The checks run when node starts the worker (see
-- check-langs.sh).
-- JavaScript の中の Elm の Int。「JavaScript に変換する」の章。
-- node でワーカーを始めると、確かめが動く（check-langs.sh を参照）

import Platform


-- ANCHOR: elm
-- Elm
factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)


checks : List Bool
checks =
    [ factorial 20 == 2432902008176640000
    -- 25! is too long for a JavaScript number: the last digits go.
    -- 25! は JavaScript の number には長すぎて、下の桁が消える
    , String.fromInt (factorial 25) == "1.5511210043330986e+25"
    , -7 // 2 == -3         -- toward 0, as JavaScript's | 0
    , modBy 2 -7 == 1       -- the sign of 2
    , remainderBy 2 -7 == -1 -- the sign of -7
    ]
-- ANCHOR_END: elm


main : Program () () ()
main =
    if List.all identity checks then
        Platform.worker
            { init = \_ -> ( (), Cmd.none )
            , update = \_ _ -> ( (), Cmd.none )
            , subscriptions = \_ -> Sub.none
            }

    else
        Debug.todo "a check of Division.elm failed"
