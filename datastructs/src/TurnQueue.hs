-- ANCHOR: all
-- 🔄 The turn order of a battle, as a Seq.
-- バトルの行動順（Seq で）
module TurnQueue (Hero, takeTurns, joinFront, joinBack) where

import Data.Sequence (Seq (..), (<|), (|>))

type Hero = String

-- ANCHOR: turns
-- 🔄 Let n heroes act in order. Who acts goes to the back of the line.
-- n 人ぶん、順に行動させる。行動した人は、列の後ろに並び直す
takeTurns :: Int -> Seq Hero -> [Hero]
takeTurns 0 _     = []
takeTurns _ Empty = []
takeTurns n (hero :<| rest) = hero : takeTurns (n - 1) (rest :|> hero)

-- ⏩ Someone fast cuts in at the front; someone late joins at the back.
-- 素早い人は先頭に割り込み、遅れてきた人は最後尾に並ぶ
joinFront, joinBack :: Hero -> Seq Hero -> Seq Hero
joinFront hero line = hero <| line
joinBack  hero line = line |> hero
-- ANCHOR_END: turns
-- ANCHOR_END: all
