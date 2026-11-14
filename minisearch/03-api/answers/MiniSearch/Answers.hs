-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the Web API chapter.
-- Web API の章のトレーニングの解答。
module MiniSearch.Answers
  ( Offset
  , offsetValue
  , parseOffset
  , page
  , maxOffset
  ) where
-- ANCHOR_END: head

-- ANCHOR: offset
-- 📄 Where a page starts: 0 to maxOffset.
-- ページの始まり：0 から maxOffset まで
newtype Offset = Offset Int
  deriving (Eq, Show)

-- 🔢 Read the number (no field: a field could change it from outside).
-- 数を読む（フィールドにすると、外から書きかえられる）
offsetValue :: Offset -> Int
offsetValue (Offset n) = n

maxOffset :: Int
maxOffset = 1000

-- 🚪 No offset given -> 0. Out of range -> Left with a message.
-- 指定がなければ 0。範囲の外なら、メッセージ付きの Left
parseOffset :: Maybe Int -> Either String Offset
parseOffset Nothing = Right (Offset 0)
parseOffset (Just n)
  | n < 0 || n > maxOffset =
      Left ("offset must be 0 to " <> show maxOffset <> ": " <> show n)
  | otherwise = Right (Offset n)

-- ✂️ Skip `offset` items, then take `size` items.
-- offset 個とばして、size 個取る
page :: Offset -> Int -> [a] -> [a]
page (Offset o) size xs = take size (drop o xs)
-- ANCHOR_END: offset
-- ANCHOR_END: all
