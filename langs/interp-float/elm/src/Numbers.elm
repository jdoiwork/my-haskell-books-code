module Numbers exposing (main)

-- The numbers of Elm, for the chapter on floats of Lam.
-- The checks run when node starts the worker (see check-langs.sh).
-- Elm の数。Lam の小数の章。
-- node でワーカーを始めると、確かめが動く（check-langs.sh を参照）

import Platform


-- ANCHOR: average
-- Elm
average : List Int -> Float
average xs =
    toFloat (List.sum xs) / toFloat (List.length xs)


double : number -> number
double n =
    n * 2
-- ANCHOR_END: average


checks : List Bool
checks =
    [ average [ 82, 75, 91, 64 ] == 78
    , double 4 == 8
    , double 1.5 == 3
    -- ANCHOR: surprises
    , 7 // 2 == 3
    , 7 // 0 == 0          -- not an error: 0
    , round -2.5 == -2     -- a half goes up, as Math.round
    , String.fromFloat 1.0 == "1"
    -- ANCHOR_END: surprises
    , isNaN (0 / 0)
    ]


main : Program () () ()
main =
    if List.all identity checks then
        Platform.worker
            { init = \_ -> ( (), Cmd.none )
            , update = \_ _ -> ( (), Cmd.none )
            , subscriptions = \_ -> Sub.none
            }

    else
        Debug.todo "a check of Numbers.elm failed"
