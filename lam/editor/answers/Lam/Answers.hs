-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Writing in an editor: colors and
--   LSP".
-- 「エディタで書く：色付けと LSP」のトレーニングの解答。
module Lam.Answers
  ( completeAt
  , definitionAt
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, (<.>), (</>))

import Lam.Check (columnToIndex, wordAt)
import Lam.Module (Files (..))
import Lam.Program (Session)
import Lam.Repl (completions)

-- ANCHOR: completeAt
-- ⌨️ Step 2: the names to offer at a place: those of the session (the
--    same as Tab in the REPL) that start with the part of the word
--    before the place. The commands of the REPL (:load) are left out.
-- ステップ2：場所で勧める名前。セッションの名前（REPL の Tab と同じ）の
-- うち、場所より前の語のかけらで始まるもの。REPL のコマンド（:load）は
-- 外す
completeAt :: Session -> Text -> Int -> Int -> [Text]
completeAt session source line column =
  [ n | n <- completions session, not (":" `T.isPrefixOf` n)
      , typed `T.isPrefixOf` n ]
  where
    text = maybe "" id (listToMaybe (drop (line - 1) (T.lines source)))
    before = T.take (columnToIndex text column) text
    typed = T.takeWhileEnd (\ch -> ch == '.' || ch == '_' || isWord ch)
              before
    isWord ch = ch `elem` ['a' .. 'z'] || ch `elem` ['A' .. 'Z']
                  || ch `elem` ['0' .. '9']
-- ANCHOR_END: completeAt

-- ANCHOR: definitionAt
-- 🎯 Step 3: where the name under a place is defined: a line of this
--    file that starts with it, or else a line of a module this file
--    imports (Price.lam next to it, for import Price). The file and
--    the line; Nothing for a name of the prelude, or of nowhere.
-- ステップ3：場所の下の名前を定義した所。このファイルの、その名前で始まる
-- 行か、なければ、このファイルが import するモジュール（import Price なら
-- 隣の Price.lam）の行。ファイルと行。プレリュードの名前や、どこにもない
-- 名前なら Nothing
definitionAt
  :: Monad m => Files m -> FilePath -> Text -> Int -> Int
  -> m (Maybe (FilePath, Int))
definitionAt files file source line column =
  case wordAt source line column of
    Nothing -> pure Nothing
    Just (_, word) -> case definedIn word source of
      Just n  -> pure (Just (file, n))
      Nothing -> firstJust (map (inModule word) (importedIn source))
  where
    inModule word m = do
      let path = takeDirectory file </> T.unpack m <.> "lam"
      found <- readSource files path
      pure (found >>= fmap (\n -> (path, n)) . definedIn (plain word))
    -- Price.yen is yen in Price.lam.
    -- Price.yen は、Price.lam の yen
    plain word = T.takeWhileEnd (/= '.') word

-- 📏 The line that defines x: it starts with x, then a space, a : or
--    a = (as definitionLine of Lam.Check, but Nothing if none).
-- x を定義する行（Lam.Check の definitionLine と同じ。なければ Nothing）
definedIn :: Text -> Text -> Maybe Int
definedIn x source = listToMaybe
  [ n | (n, l) <- zip [1 ..] (T.lines source)
      , Just after <- [T.stripPrefix x l]
      , T.take 1 after `elem` ["", " ", ":", "="] ]

-- 📥 The modules a file imports: import Price (...) gives Price.
-- ファイルが import するモジュール
importedIn :: Text -> [Text]
importedIn source = [ m | ("import" : m : _) <- map T.words (T.lines source) ]

-- 🥇 The first answer that is there, trying one after another.
-- 順に試して、最初にあった答え
firstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJust tries = case tries of
  [] -> pure Nothing
  t : rest -> t >>= maybe (firstJust rest) (pure . Just)
-- ANCHOR_END: definitionAt
-- ANCHOR_END: all
