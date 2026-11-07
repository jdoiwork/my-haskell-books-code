module Elements exposing (main)

-- The type chart of elements, for the chapter "A map of languages".
-- The checks run when node starts the worker (see check-langs.sh).
-- 属性の相性。「言語の地図」の章。
-- node でワーカーを始めると、確かめが動く（check-langs.sh を参照）


-- ANCHOR: elements
-- Elm
type Element
    = Fire
    | Water
    | Grass

beats : Element -> Element -> Bool
beats attacker defender =
    case ( attacker, defender ) of
        ( Fire, Grass ) -> True
        ( Water, Fire ) -> True
        ( Grass, Water ) -> True
        _ -> False
-- ANCHOR_END: elements


checks : List Bool
checks =
    [ beats Fire Grass
    , not (beats Grass Fire)
    , not (beats Water Water)
    ]


-- Stops with an error when a check fails.
-- 確かめが1つでも外れたら、エラーで止まる
main : Program () () ()
main =
    Platform.worker
        { init =
            \_ ->
                if List.all identity checks then
                    ( (), Cmd.none )

                else
                    Debug.todo "wrong result"
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
