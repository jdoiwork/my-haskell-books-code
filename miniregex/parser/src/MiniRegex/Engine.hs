-- ANCHOR: all
-- | The engine. compilePattern now reads with the real parser; the
--   matcher is still the stand-in of the design chapter (plain
--   characters, T.breakOn), so a tree it cannot run is stopped here,
--   as "not yet". The next chapter swaps searchFrom and takes the
--   guard away; the two functions keep their types.
-- エンジン。compilePattern は、本物のパーサーで読むようになった。
-- マッチャーは、まだ設計の章の仮のもの（ふつうの文字、T.breakOn）なので、
-- 動かせない木は、ここで「まだ」として止める。次の章で searchFrom を
-- 取り替え、見張りを外す。2つの関数の型は、そのまま
module MiniRegex.Engine
  ( compilePattern
  , searchFrom
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Parser (parsePattern)
import MiniRegex.Syntax
import MiniRegex.Types
-- ANCHOR_END: imports

-- ANCHOR: compile
-- 🏗️ Read a pattern into a tree, with the parser; then let only the
--    trees that the stand-in matcher can run go on.
-- パーサーでパターンを読んで木にする。そのあと、仮のマッチャーが
-- 動かせる木だけを通す
compilePattern :: [Option] -> Text -> Either RegexError Pattern
compilePattern opts src = do
  tree <- parsePattern opts src
  standInCanRun opts tree
  pure tree

-- 🚧 The stand-in runs one branch of plain characters, with no flags.
--    The tree keeps no places, so the place is 0, the whole pattern.
-- 仮のマッチャーが動かせるのは、ふつうの文字を並べた1本の枝だけ。
-- フラグもなし。木は場所を覚えていないので、場所は 0（パターン全体）
standInCanRun :: [Option] -> Pattern -> Either RegexError ()
standInCanRun (_ : _) _ = Left (Unsupported 0 "matcher: flags")
standInCanRun [] (Pattern (Branch pieces :| [])) = mapM_ plain pieces
  where
    plain (Quantified (Lit _) q) | q == once = Right ()
    plain p = Left (Unsupported 0 ("matcher: " <> kind p))
standInCanRun [] _ = Left (Unsupported 0 "matcher: |")

-- 🏷️ What kind of piece the stand-in cannot run.
-- 仮のマッチャーが動かせない部品の種類
kind :: Piece -> Text
kind (Anchor LineStart) = "^"
kind (Anchor LineEnd)   = "$"
kind (Quantified a q)
  | q /= once = "quantifier"
  | otherwise = case a of
      AnyChar   -> "."
      Class _   -> "[]"
      Group _ _ -> "()"
      Lit _     -> "?"
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
