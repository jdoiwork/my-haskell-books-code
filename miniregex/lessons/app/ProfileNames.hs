-- ANCHOR: all
-- | Run the backtracking VM with ST on the name field, many times, for
--   the profiler: where do the steps spend their time?
-- 名前の欄で、ST のバックトラックの VM を何度も動かす。プロファイラーで、
-- 手数がどこで時間を使っているかを見るため
module Main (main) where

import Control.Monad (forM_)
import Control.Exception (evaluate)
import Data.Text qualified as T

import System.Environment (getArgs)

import MiniRegex
import MiniRegex.Engine (searchCountingBy)
import MiniRegex.Examples (guildBook, surely)
import MiniRegex.STVM (scanVMST)

-- 🔬 Two hundred times, each with a fresh copy of the field. With
--    "dates": findAll on the guild's book, ten times.
-- 200 回。毎回、欄の新しい写しで。"dates" なら、ギルドの帳面に findAll
-- を10回
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["dates"] -> dates
    _         -> nameField

dates :: IO ()
dates = forM_ [1 .. 10 :: Int] $ \_ -> do
  book <- evaluate (guildBook 20000)
  evaluate (length (findAll (surely "\\d{4}-\\d{2}-\\d{2}") book))

nameField :: IO ()
nameField = forM_ [1 .. 200 :: Int] $ \_ -> do
  field <- evaluate (T.copy "Mira Ruby Lam GHC Perl!")
  let answer = fst (searchCountingBy scanVMST defaultLimit names field 0)
  evaluate (either (error . show) (fmap matchEnd) answer)
  where
    names = surely "^(\\w+\\s?)+$"
-- ANCHOR_END: all
