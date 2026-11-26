-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | The answers of the training of the chapter on polishing.
-- 道具に仕上げる章のトレーニングの解答
module PolishAnswers
  ( wholeLine
  , reWith
  , nocase
  , yearNumber
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Records (HasField)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)

import MiniRegex
import MiniRegex.Parser (parsePattern)
import MiniRegex.Pretty (render)
import MiniRegex.Quote (re)
import MiniRegex.Quoted (readInt)
import MiniRegex.Typed (TypedMatch)
-- ANCHOR_END: head

-- ANCHOR: whole
-- 📏 Step 1: -x, the whole line. The pattern goes in a group that
--    keeps nothing, so that a | inside stays inside. It is read and
--    written back first: a \Q with no \E, or a # comment under (?x),
--    would swallow the ) put after it. A wrong pattern is left as it
--    is, for compile to tell.
-- ステップ1：-x、行の全体。パターンを、何も覚えないグループに入れる
-- （中の | が、外へもれないように）。先に読んで書き戻す：\E のない \Q や、
-- (?x) の # のコメントは、うしろに付けた ) を飲みこんでしまう。まちがった
-- パターンは、compile が知らせるように、そのまま返す
wholeLine :: Text -> Text
wholeLine p = case parsePattern [] p of
  Left _     -> p
  Right tree -> "^(?:" <> render tree <> ")$"
-- ANCHOR_END: whole

-- ANCHOR: rewith
-- 🚩 Step 2: a quasi-quoter with flags. The flags go into the code as
--    numbers (Option is an Enum), and come back with toEnum.
-- ステップ2：フラグつきの準クォート。フラグは数としてコードに入れ
-- （Option は Enum）、toEnum で戻す
reWith :: [Option] -> QuasiQuoter
reWith opts = re { quoteExp = quote }
  where
    quote :: String -> Q Exp
    quote s = case compileWith opts (T.pack s) of
      Left err ->
        fail ("bad pattern:\n" <> T.unpack (explain (T.pack s) err))
      Right _  ->
        [| either (error . show) id
             (compileWith (map toEnum $(lift (map fromEnum opts)))
                          (T.pack s)) |]

-- 🔡 [nocase|perl|]: the same as [re|(?i)perl|].
-- [nocase|perl|]：[re|(?i)perl|] と同じ
nocase :: QuasiQuoter
nocase = reWith [Caseless]
-- ANCHOR_END: rewith

-- ANCHOR: year
-- 📅 Step 3: the year as a number, for any typed regex whose "year" is
--    always set.
-- ステップ3：年を数で。year が必ず使われる、どの型付きの正規表現でも
yearNumber :: HasField "year" (TypedMatch gs) Text
           => TypedMatch gs -> Maybe Int
yearNumber m = readInt m.year
-- ANCHOR_END: year
-- ANCHOR_END: all
