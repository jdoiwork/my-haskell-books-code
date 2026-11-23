{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | Regular expressions for Lam, on top of the Haskell package pcre2
--   (PCRE2, the C library, comes inside it). This module knows nothing
--   about Lam: it compiles a pattern, and cuts a text at its matches.
-- Lam の正規表現の土台。Haskell のパッケージ pcre2 の上に作る
-- （C のライブラリ PCRE2 は、パッケージの中に入っている）。
-- このモジュールは Lam を知らない。パターンをコンパイルし、
-- 文字列を、見つかったところで切るだけ。
module Lam.Pcre
  ( Regex (..)
  , Cut (..)
  , compileRegex
  ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate, try)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Pcre2
  ( Option (SubGlobal, SubReplacementOnly, SubUnsetEmpty, Ucp)
  , SomePcre2Exception, predictCapturesInfo, subOpt )
-- ANCHOR_END: head

-- ANCHOR: pattern
-- 🧩 A compiled pattern: the pattern as it was written, how many
--    groups it has, the names of its groups ((?<year>...) is group 1,
--    named "year"), and the search. The search keeps PCRE2's compiled code inside it, so it
--    is compiled once, however many texts we search.
-- コンパイルしたパターン：書いたとおりのパターン、グループの数、
-- グループの名前（(?<year>...) は1番のグループで、名前は year）、探す関数。
-- 探す関数は、PCRE2 がコンパイルしたものを中に持っているので、
-- 何回探しても、コンパイルは1回だけ
data Regex = Regex
  { regexSource :: Text
  , regexGroups :: Int
  , regexNames  :: [(Text, Int)]
  , regexSearch :: Text -> Either Text Cut
  }

-- 🔪 A text, cut at the matches. n matches leave n + 1 pieces around
--    them. Each match is its whole text and its groups:
--    \d+ in "a1b22c" is  Cut ["a", "b", "c"] ["1" :| [], "22" :| []].
-- 見つかったところで切った文字列。n 個見つかれば、まわりの切れ端は
-- n + 1 個。見つかったものは、全体と、グループたち
data Cut = Cut [Text] [NonEmpty Text]
-- ANCHOR_END: pattern

-- ANCHOR: compile
-- 🛠️ Compile a pattern, or say what is wrong with it.
--    pcre2 throws a mistake as an exception, even from pure code (like
--    head [] does), so we catch it here, once, and turn it into Left.
--    unsafePerformIO is safe here: compiling the same pattern always
--    gives the same answer, and touches nothing outside.
-- パターンをコンパイルする。まちがいなら、どこがまちがいかを返す。
-- pcre2 は、純粋なコードからでも、まちがいを例外で投げる（head [] と
-- 同じ）。ここで一度だけ受け止めて、Left にする。unsafePerformIO を
-- 使っても安全：同じパターンなら、いつも同じ答えで、外の何にも触れない
compileRegex :: Text -> Either Text Regex
compileRegex source = unsafePerformIO $ do
  info <- try (predictCapturesInfo unicode source)
  pure $ case info of
    Left err         -> Left (firstLine err)
    Right (groups, names) ->
      Right (Regex source groups names (cutAt source groups))

-- 🌏 \w, \d, \s and \b go by Unicode, as in Python: \w+ finds "小さな"
--    too, not only "Lam". (PCRE2 alone looks at ASCII only.)
-- \w・\d・\s・\b を Unicode で決める（Python と同じ）。\w+ は "Lam" だけ
-- でなく "小さな" も見つける（PCRE2 の既定は ASCII だけ）
unicode :: Option
unicode = Ucp
-- ANCHOR_END: compile

-- ANCHOR: cutAt
-- ✂️ Cut a text at the matches of a pattern.
--    The pcre2 API tells what it found, but not where. So we ask
--    PCRE2's own replacing (pcre2_substitute) twice, with letters that
--    are not in the text as marks (from U+E000, the private area of
--    Unicode):
--      1. put the mark P in place of every match, and cut at P
--         ("a1b22c" -> "aPbPc" -> ["a", "b", "c"])
--      2. write out only the matches, as  whole G group1 G ... E
--         ("1E22E" -> [["1"], ["22"]])
-- 文字列を、パターンが見つかったところで切る。
-- pcre2 の API は、見つかったものはくれるが、場所はくれない。そこで、
-- 文字列に出てこない文字（Unicode の私用の領域 U+E000 から）を印にして、
-- PCRE2 自身の置き換え（pcre2_substitute）に2回頼む：
--   1. 見つかったところを印 P に置き換えて、P で切る
--   2. 見つかったものだけを「全体 G グループ1 G ... E」の形で書き出す
cutAt :: Text -> Int -> Text -> Either Text Cut
cutAt source groups = \subject -> unsafePerformIO $ do
  -- ANCHOR: usualMarks
  -- The usual marks are 3 letters in a row (U+E000 to U+E002): one
  -- look at each letter, not 3 (elem on a list asked == 3 times; in
  -- the profile of a program that called Regex.find for each line, ==
  -- and elem took a third of the time. See the chapter on making Lam
  -- fast).
  -- いつもの印は、並んだ3文字（U+E000〜U+E002）。1文字を1回見るだけで
  -- すむ（リストの elem は == を3回聞く。1行ごとに Regex.find を呼ぶ
  -- プログラムのプロファイルで、== と elem が時間の3割を使っていた。
  -- Lam を速くするの章を見る）
  let (a, _, c) = defaultMarks
      usual     = not (T.any (\ch -> ch >= a && ch <= c) subject)
      run       = if usual then cutUsual else cutWith (marksFor subject)
  -- ANCHOR_END: usualMarks
  result <- try (evaluate (force (run subject)))
  pure $ case result of
    Left err           -> Left (firstLine err)
    Right (out, found) -> Right (Cut out found)
  where
    -- Made once for the pattern, and shared by every subject: PCRE2's
    -- compiled code is kept inside. Only a text that has the usual
    -- marks in it needs other marks, and compiles again.
    -- パターンごとに1回だけ作り、どの文字列にも使い回す（PCRE2 が
    -- コンパイルしたものは、この中にある）。いつもの印を含む文字列の
    -- ときだけ、別の印でコンパイルしなおす
    cutUsual = cutWith defaultMarks

    cutWith (mp, mg, me) =
      let (p, g, e) = (T.singleton mp, T.singleton mg, T.singleton me)
          -- ${0}G${1}G${2}E: every group, unset ones as "".
          -- すべてのグループ。使われなかったものは ""
          wanted = T.intercalate g [ "${" <> T.pack (show i) <> "}"
                                   | i <- [0 .. groups] ] <> e
          pieces  = subOpt (unicode <> SubGlobal) source p
          matches = subOpt (unicode <> SubGlobal <> SubReplacementOnly
                              <> SubUnsetEmpty) source wanted
      in \subject ->
           ( T.splitOn p (pieces subject)
           , [ w :| gs
             | one <- dropLast (T.splitOn e (matches subject))
             , w : gs <- [T.splitOn g one] ] )

    dropLast xs = take (length xs - 1) xs

-- 🔣 The usual marks, and other marks for a text that has them:
--    the first 3 letters from U+E000 on that are not in the text.
-- いつもの印と、それを含む文字列のための別の印：U+E000 から数えて、
-- 文字列に出てこない最初の3文字
defaultMarks :: (Char, Char, Char)
defaultMarks = ('\xE000', '\xE001', '\xE002')

marksFor :: Text -> (Char, Char, Char)
marksFor subject =
  case [ c | c <- ['\xE000' ..], not (T.any (== c) subject) ] of
    a : b : c : _ -> (a, b, c)
    -- A text with almost every letter of Unicode in it.
    -- Unicode のほぼすべての文字を含む文字列（まず、ありえない）
    _             -> error "no letters are left to mark the matches"
-- ANCHOR_END: cutAt

-- 📣 "pcre2_compile: missing closing parenthesis" and the lines after
--    it become "missing closing parenthesis".
-- 例外の最初の行から、PCRE2 の文だけを取り出す
firstLine :: SomePcre2Exception -> Text
firstLine err =
  let line = T.takeWhile (/= '\n') (T.pack (show err))
  in T.strip (T.drop 1 (T.dropWhile (/= ':') line))
-- ANCHOR_END: all
