-- ANCHOR: all
-- | Reading numbers carefully. A pure part of the core: both the
--   command line (Cli) and the settings (Config) use it.
-- 数を、ていねいに読む。純粋な核の部品：コマンドライン（Cli）と
-- 設定（Config）の両方が使う。
module MiniSearch.Number (wholeNumber) where

import Data.Char (isDigit)

-- 🔢 Digits only, and small enough for an Int. "0x5", " 7" and numbers
--    too big for an Int are not numbers here ('reads' would take them).
-- 数字だけで、Int に入る大きさの数。「0x5」「 7」や、Int に入らない数は、
-- ここでは数にしない（'reads' なら読んでしまう）
wholeNumber :: String -> Maybe Int
wholeNumber s
  | null s || not (all isDigit s)  = Nothing
  | n > toInteger (maxBound :: Int) = Nothing
  | otherwise                       = Just (fromInteger n)
  where
    n = read s :: Integer
-- ANCHOR_END: all
