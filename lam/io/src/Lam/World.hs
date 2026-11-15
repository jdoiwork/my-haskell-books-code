-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Two worlds for the runner: the real one (the screen, the keyboard
--   and the disk), and a fake one for the tests (lines and files kept
--   in a Haskell value).
-- 実行役の2つの世界：本物（画面、キーボード、ディスク）と、
-- テスト用の偽物（行とファイルを、Haskell の値に持つ）。
module Lam.World
  ( realWorld
  , FakeWorld (..)
  , emptyWorld
  , fakeWorld
  , runFake
  , outputOf
  ) where

import Control.Exception (IOException, try)
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.IO (hFlush, isEOF, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import Lam.Builtins (con)
import Lam.Eval
-- ANCHOR_END: head

-- ANCHOR: real
-- 🌍 The real world: here, at last, Haskell's IO does the work.
-- 本物の世界：ここで、やっと Haskell の IO が仕事をする
realWorld :: Request -> IO Value
realWorld request = case request of
  PutStr s -> do
    TIO.putStr s
    hFlush stdout                -- show "name? " before waiting
    pure unit
  GetLine -> do
    end <- isEOF
    if end
      then pure (con "Nothing" [])
      else do
        line <- TIO.getLine
        pure (con "Just" [VString line])
  ReadFile path -> do
    result <- try (Utf8.readFile (T.unpack path))
    pure (resultOf path (VString <$> result))
  WriteFile path text -> do
    result <- try (Utf8.writeFile (T.unpack path) text)
    pure (resultOf path (unit <$ result))

-- 📦 Ok v, or Err with a short message for people.
-- Ok v か、人が読む短い文を入れた Err
resultOf :: Text -> Either IOException Value -> Value
resultOf path result = case result of
  Right v -> con "Ok" [v]
  Left e  -> con "Err" [VString (path <> ": " <> reason e)]
  where
    reason e
      | isDoesNotExistError e = "no such file"           -- 「ファイルがない」
      | isPermissionError e   = "permission denied"      -- 「許されていない」
      | otherwise             = T.pack (show e)

unit :: Value
unit = VTuple []
-- ANCHOR_END: real

-- ANCHOR: fake
-- 🎭 A fake world: the lines to type, the files, and the screen.
-- 偽物の世界：打ちこむ行、ファイル、画面
data FakeWorld = FakeWorld
  { worldInput  :: [Text]          -- the lines still to be read
  , worldFiles  :: Map Text Text   -- the name and the text of each file
  , worldOutput :: [Text]          -- what was written, the newest first
  }
  deriving (Eq, Show)

emptyWorld :: FakeWorld
emptyWorld = FakeWorld [] Map.empty []

-- 🎭 The fake handler: the same requests, the same answers, but every
--    one is only a change to a Haskell value. No disk, no keyboard.
-- 偽物のハンドラー：同じ頼みごとに、同じ形の答え。でも、どれも
-- Haskell の値を書き換えるだけ。ディスクにもキーボードにも触れない
fakeWorld :: Request -> State FakeWorld Value
fakeWorld request = case request of
  PutStr s -> do
    modify' (\w -> w { worldOutput = s : worldOutput w })
    pure unit
  GetLine -> do
    input <- gets worldInput
    case input of
      [] -> pure (con "Nothing" [])
      line : rest -> do
        modify' (\w -> w { worldInput = rest })
        pure (con "Just" [VString line])
  ReadFile path -> do
    found <- gets (Map.lookup path . worldFiles)
    pure $ case found of
      Just text -> con "Ok" [VString text]
      Nothing   -> con "Err" [VString (path <> ": no such file")]
  WriteFile path text -> do
    modify' (\w -> w { worldFiles = Map.insert path text (worldFiles w) })
    pure (con "Ok" [unit])

-- ▶️ Run something in a fake world, and see the world afterwards.
-- 偽物の世界で動かし、そのあとの世界を見る
runFake :: FakeWorld -> State FakeWorld a -> (a, FakeWorld)
runFake world m = runState m world

-- 🖥️ Everything written to the screen, in order.
-- 画面に書いたもの全部を、順に
outputOf :: FakeWorld -> Text
outputOf = T.concat . reverse . worldOutput
-- ANCHOR_END: fake
-- ANCHOR_END: all
