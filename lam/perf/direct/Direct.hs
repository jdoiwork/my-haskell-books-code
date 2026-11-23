{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The programs of examples/perf, written in Haskell itself: how fast
--   the same work can be, with no interpreter in between.
--   lam-direct loop | fib | range | sort | count FILE | slow FILE | deep
-- examples/perf のプログラムを、Haskell で直接書いたもの。あいだに
-- インタープリターがないと、同じ仕事がどこまで速くなるか
module Main (main) where

import Data.List (intercalate, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode (..), hIsEOF, withFile)

-- ANCHOR: loop
-- 🔁 The loop of loop.lam: a function that calls itself last.
-- loop.lam のループ。最後に自分を呼ぶ関数
countUp :: Integer -> Integer -> Integer
countUp total n
  | n > 1000000 = total
  | otherwise   = countUp (total + n) (n + 1)
-- ANCHOR_END: loop

-- 🐇 fib of fib.lam.
-- fib.lam の fib
fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- 📏 Count the lines, one at a time.
-- 行を1行ずつ数える
countLines :: FilePath -> IO Int
countLines path = withFile path ReadMode (go 0)
  where
    go !n h = do
      done <- hIsEOF h
      if done then pure n else TIO.hGetLine h >> go (n + 1) h

-- 🐢 slow.lam without a regular expression: the fields, by hand.
-- 正規表現なしの slow.lam：欄を手で切り出す
slowCounts :: T.Text -> Map.Map T.Text Int
slowCounts = foldl' step Map.empty . T.lines
  where
    step acc l = case reverse (T.words l) of
      rtField : _ : statusField : _
        | Just rt <- T.stripPrefix "rt=" rtField
        , Right (st, _) <- int (TR.decimal statusField)
        , Right (t, _) <- TR.double rt
        , st >= 500 && t > 0.5 ->
            Map.insertWith (+) (T.takeWhile (/= ' ') l) 1 acc
      _ -> acc
    int :: Either String (Int, T.Text) -> Either String (Int, T.Text)
    int = id

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["loop"]  -> print (countUp 0 1)
    ["fib"]   -> print (fib 27)
    ["range"] -> print (foldl' (+) 0 [1 .. 1000000 :: Integer])
    ["sort"]  -> do
      let sorted = sort [ (i * 7919) `mod` 100003
                        | i <- [1 .. 100000 :: Integer] ]
      -- Written as Lam writes a list: [1, 2, 3]
      -- Lam と同じ書き方のリスト
      putStrLn ("[" <> intercalate ", " (map show (take 5 sorted)) <> "]")
      print (length sorted)
    ["count", path] -> countLines path >>= print
    ["slow", path] -> do
      counts <- slowCounts <$> TIO.readFile path
      let top = take 5 (sortOn (negate . snd) (Map.toList counts))
      mapM_ (\(ip, n) -> putStrLn (pad (show n) <> "  " <> T.unpack ip)) top
    ["deep"] -> do
      print (countUp 0 1)
      let xs = [1 .. 200000 :: Integer]
      print (sum (map (* 2) xs))
      print (length (sort (reverse xs)))
    _ -> die "usage: lam-direct loop|fib|range|sort|count FILE|slow FILE|deep"
  where
    pad s = replicate (5 - length s) ' ' <> s
-- ANCHOR_END: all
