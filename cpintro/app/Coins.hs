-- ANCHOR: all
-- 🪙 Problem 4: read N and X, then N coins on one line.
--    Can you pay exactly X gold with some of the coins?
--    Print "Yes" or "No".
-- 問題4：N と X を読み、次の行の N 枚の金貨を読む
-- 何枚か選んで、ちょうど X ゴールドを払えるか
-- "Yes" か "No" を表示する
main :: IO ()
main = do
  -- line 1: "5 15" (N and X). We use the second word, X.
  -- 1行目："5 15"（N と X）。2番目の単語の X を使う
  firstLine <- getLine
  -- line 2: "3 8 4 2 9"
  -- 2行目："3 8 4 2 9"
  coinsLine <- getLine
  let target = read (words firstLine !! 1) :: Int
      coins  = map read (words coinsLine) :: [Int]
  putStrLn (if canPay target coins then "Yes" else "No")

-- ANCHOR: canPay
-- 💰 Can we pay exactly `target` with some of the coins?
--    For each coin there are only two choices: use it, or skip it.
-- 金貨を何枚か選んで、ちょうど target を払えるか
-- 1枚ごとの選択肢は2つだけ：使うか、使わないか
canPay :: Int -> [Int] -> Bool
canPay target []            = target == 0
canPay target (coin : rest) =
  -- use this coin, or skip it
  -- この金貨を使う、または使わない
  canPay (target - coin) rest || canPay target rest
-- ANCHOR_END: canPay
-- ANCHOR_END: all
