-- ⚠️ This file does NOT compile, on purpose (the Monad intro chapter).
--    It is not in monadintro.cabal.
-- わざとコンパイルできないファイル（Monad 入門の章）。
-- monadintro.cabal には入れていない
module Mixed (f) where

-- ANCHOR: mixed
-- ❌ IO, a list, and a Maybe in one do: they are different boxes.
-- IO とリストと Maybe を1つの do に：別々の箱なので、混ぜられない
f :: [Int] -> Maybe Int -> IO (Int, Int, String)
f xs y = do
  line <- getLine
  x    <- xs
  y'   <- y
  pure (x, y', line)
-- ANCHOR_END: mixed
