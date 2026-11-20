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
import Text.Read (readMaybe)

import MiniRegex
import MiniRegex.Quote (re)
import MiniRegex.Typed (TypedMatch)
-- ANCHOR_END: head

-- ANCHOR: whole
-- 📏 Step 1: -x, the whole line. The pattern goes in a group that
--    keeps nothing, so that a | inside stays inside.
-- ステップ1：-x、行の全体。パターンを、何も覚えないグループに入れる
-- （中の | が、外へもれないように）
wholeLine :: Text -> Text
wholeLine p = "^(?:" <> p <> ")$"
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
yearNumber m = readMaybe (T.unpack m.year)
-- ANCHOR_END: year
-- ANCHOR_END: all
