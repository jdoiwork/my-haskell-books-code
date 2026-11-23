{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | Regular expressions for Lam, on top of MiniRegex (the engine of
--   the regex part of this book). The same names as Lam.Pcre, so
--   Lam.Regex and Lam.Parser only change their import.
-- Lam の正規表現の土台を、MiniRegex（第13部で作ったエンジン）の上に。
-- 名前は Lam.Pcre と同じなので、Lam.Regex と Lam.Parser は import だけ
-- を書き換える
module Lam.MiniRegex
  ( Regex (..)
  , Cut (..)
  , compileRegex
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified MiniRegex as M
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Types (GaveUp (..), RegexError (..), tree)
-- ANCHOR_END: head

-- ANCHOR: pattern
-- 🧩 A compiled pattern: the same four fields as in Lam.Pcre.
-- コンパイルしたパターン。Lam.Pcre と同じ4つの欄
data Regex = Regex
  { regexSource :: Text
  , regexGroups :: Int
  , regexNames  :: [(Text, Int)]
  , regexSearch :: Text -> Either Text Cut
  }

-- 🔪 A text, cut at the matches (the same as in Lam.Pcre).
-- 見つかったところで切った文字列（Lam.Pcre と同じ）
data Cut = Cut [Text] [NonEmpty Text]
-- ANCHOR_END: pattern

-- ANCHOR: compile
-- 🛠️ Compile a pattern, or say what is wrong with it. MiniRegex tells
--    a mistake as a value (Left), so there is nothing to catch.
--    The groups and their names come from the tree of the pattern.
-- パターンをコンパイルする。MiniRegex は、まちがいを値（Left）で返すので、
-- 受け止める例外はない。グループの数と名前は、パターンの木から数える
compileRegex :: Text -> Either Text Regex
compileRegex source = case M.compile source of
  Left err -> Left (message err)
  Right re ->
    Right (Regex source (groupCount (tree re)) (groupNames (tree re))
                 (cutAt re))
-- ANCHOR_END: compile

-- ANCHOR: cutAt
-- ✂️ Cut a text at the matches. MiniRegex tells where each match is,
--    so no marks are needed: the pieces are what M.split gives.
--    A search that takes too many steps gives up, as PCRE2 does.
-- 文字列を、見つかったところで切る。MiniRegex は場所を教えてくれるので、
-- 印はいらない。切れ端は M.split と同じもの。ステップ数を使いすぎた
-- 探しものは、PCRE2 と同じく、あきらめる
cutAt :: M.Regex -> Text -> Either Text Cut
cutAt re subject = case M.findAllWithin M.defaultLimit re subject of
  Left gaveUp   -> Left (giveUp gaveUp)
  Right matches -> Right (Cut (pieces matches) (map found matches))
  where
    pieces matches = go 0 matches
      where
        go at []       = [T.drop at subject]
        go at (m : ms) = slice at (M.matchStart m) : go (M.matchEnd m) ms
    slice from to = T.take (to - from) (T.drop from subject)

    -- every group, a group that took no part as ""
    -- すべてのグループ。使われなかったものは ""
    found m = M.matchText m :| map (fromMaybe "") (M.groups m)
-- ANCHOR_END: cutAt

-- ANCHOR: messages
-- 📣 The words for a mistake. MiniRegex's words are PCRE2's, so most
--    of them read as in Lam.Pcre.
-- まちがいの文。MiniRegex の文は PCRE2 と同じなので、たいていは
-- Lam.Pcre と同じに読める
message :: RegexError -> Text
message (Invalid _ msg)     = msg
message (Unsupported _ msg) = "not supported yet: " <> msg
message (NotLinear msg)     = msg

giveUp :: GaveUp -> Text
giveUp (GaveUp _)    = "match limit exceeded"
giveUp (Looping _ _) = "nested recursion at the same subject position"
-- ANCHOR_END: messages
-- ANCHOR_END: all
