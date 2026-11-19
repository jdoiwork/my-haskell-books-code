-- ANCHOR: all
-- | A stand-in for the engine, for this chapter only: it reads patterns
--   made of plain characters, and finds them with T.breakOn. The next
--   chapters swap the inside (a parser, then a backtracking matcher);
--   the two functions keep their types.
-- この章だけの、仮のエンジン：ふつうの文字だけのパターンを読み、
-- T.breakOn で探す。次の章から中身を取り替える（パーサー、そして
-- バックトラックのマッチャー）。2つの関数の型は、そのまま
module MiniRegex.Engine
  ( compilePattern
  , searchFrom
  ) where

-- ANCHOR: imports
import Data.Char (isAscii, isAlphaNum)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Syntax
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree. For now, only plain characters, and
--    \ before a sign (\. \$ \\ ...), which means the sign itself.
-- パターンを読んで木にする。いまは、ふつうの文字と、記号の前の \
-- （\. \$ \\ など。その記号そのもの）だけ
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern (_ : _) _ =
  Left (Unsupported 0 "flags are not supported yet")
compilePattern [] src = literal . T.pack <$> go 0 (T.unpack src)
  where
    go :: Int -> String -> Either RegexError String
    go _  [] = Right []
    -- PCRE2 tells the place just after the \ (error 101).
    -- PCRE2 は、\ のすぐうしろの場所を言う（エラー 101）
    go at ['\\'] = Left (Invalid (at + 1) "\\ at end of pattern")
    go at ('\\' : c : rest)
      | isSign c  = (c :) <$> go (at + 2) rest
      | otherwise = Left (Unsupported at ("\\" <> T.singleton c))
    go at (c : rest)
      | c `elem` metaChars = Left (Unsupported at (T.singleton c))
      | otherwise          = (c :) <$> go (at + 1) rest

-- 🔣 The characters with a meaning in a pattern.
-- パターンの中で、意味を持つ文字
metaChars :: String
metaChars = "\\^$.|?*+()[]{}"

-- ❗ An ASCII sign: \ before it means the sign itself.
-- ASCII の記号。前に \ を付けると、その記号そのものになる
isSign :: Char -> Bool
isSign c = isAscii c && not (isAlphaNum c) && c > ' '
-- ANCHOR_END: compile

-- ANCHOR: search
-- 🔦 The first match that starts at `from` or later. `from` counts
--    characters of the whole text, so that later engines can look
--    back before it (^, \b, look-behind).
-- from の位置か、それより後ろで始まる、最初の一致。from は文字列全体の
-- 何文字目かで数える（あとのエンジンが、その手前を見られるように。
-- ^、\b、後読み）
searchFrom :: Regex -> Text -> Int -> Maybe Match
searchFrom re subject from
  | from > T.length subject = Nothing
  | T.null needle           = Just (Match "" from from)
  | T.null found            = Nothing
  | otherwise               = Just (Match needle start end)
  where
    needle          = plainText (tree re)
    (before, found) = T.breakOn needle (T.drop from subject)
    start           = from + T.length before
    end             = start + T.length needle

-- 📝 The characters of a tree that compilePattern made. It only makes
--    one branch of single characters.
-- compilePattern が作った木の文字。1本の枝に、1文字ずつの部品しか作らない
plainText :: Pattern -> Text
plainText (Pattern (Branch pieces :| [])) = T.pack (map char pieces)
  where
    char (Quantified (Lit c) q) | q == once = c
    char _ = notYet
plainText _ = notYet

notYet :: a
notYet = error "the stand-in engine got a tree it cannot run"
-- ANCHOR_END: search
-- ANCHOR_END: all
