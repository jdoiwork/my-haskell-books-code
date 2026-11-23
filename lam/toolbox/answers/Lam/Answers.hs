{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Making the tools sturdier".
-- 「道具を補強する」のトレーニングの解答。
module Lam.Answers
  ( Rules (..)
  , readRules
  , mayRun
  , listIncluded
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.FilePath (splitDirectories)

import Lam.World (cleanPath)

-- ANCHOR: rules
-- 🚫 Step 2: allow and deny. A command a deny list starts never runs,
--    even when an allow list starts it too (deny is looked at first,
--    as Claude Code does).
-- ステップ2：allow と deny。deny のリストで始まるコマンドは、allow の
-- リストでも始まっていても、動かさない（Claude Code と同じく、deny を
-- 先に見る）
data Rules = Rules { ruleAllow :: [[Text]], ruleDeny :: [[Text]] }
  deriving (Eq, Show)

readRules :: B.ByteString -> Either Text Rules
readRules bytes = case A.eitherDecodeStrict bytes of
  Right (A.Object top) | Just (A.Object dry) <- KM.lookup "dryRun" top ->
    Rules <$> list (KM.lookup "allow" dry) <*> list (KM.lookup "deny" dry)
  Right (A.Object _) -> Right (Rules [] [])
  Right _ -> Left "lam.json needs an object"
  Left why -> Left (T.pack why)
  where
    list found = case found of
      Nothing          -> Right []
      Just (A.Array xs) -> traverse entry (V.toList xs)
      Just _           -> Left "allow and deny must be lists of strings"
    entry v = case v of
      A.String s | not (null (T.words s)) -> Right (T.words s)
      _ -> Left "allow and deny must be lists of commands"

mayRun :: Rules -> [Text] -> Bool
mayRun rules ws =
  not (any (`isPrefixOf` ws) (ruleDeny rules))
    && any (`isPrefixOf` ws) (ruleAllow rules)
-- ANCHOR_END: rules

-- ANCHOR: listIncluded
-- 📂 Step 3: the names right under a folder of the included files
--    (for File.list). Nothing: no included file is under it.
-- ステップ3：入れたファイルの、あるフォルダーのすぐ下の名前（File.list の
-- ため）。Nothing なら、その下に入れたファイルはない
listIncluded :: [(FilePath, B.ByteString)] -> Text -> Maybe [Text]
listIncluded files dir =
  case [ T.pack name | (f, _) <- files
                     , name : _ <- [below (splitDirectories f)] ] of
    []    -> Nothing
    names -> Just (sort (nub names))
  where
    top = case T.unpack (cleanPath dir) of
      "." -> []
      d   -> splitDirectories d
    below ps
      | take (length top) ps == top = drop (length top) ps
      | otherwise                   = []
-- ANCHOR_END: listIncluded
-- ANCHOR_END: all
