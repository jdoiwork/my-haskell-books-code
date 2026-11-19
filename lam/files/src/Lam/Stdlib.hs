-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | The toolbox, the part written in Haskell: what scripts ask of a
--   string every day (String.trim, String.split, ...), the string
--   halves of Sequence (String.reverse, String.take, String.drop),
--   Map.map, and Json.pretty. The rest of the toolbox (Foldable,
--   Functor, the list tools) is Lam, in the prelude.
-- 道具箱の、Haskell で書いた部分：スクリプトが毎日、文字列に頼むこと
-- （String.trim・String.split など）、Sequence の文字列の側
-- （String.reverse・String.take・String.drop）、Map.map、Json.pretty。
-- 道具箱の残り（Foldable・Functor・リストの道具）は、プレリュードの Lam。
module Lam.Stdlib
  ( stdlibBuiltins
  , splitOn
  ) where

import Data.Aeson.Encode.Pretty
  (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)

import Lam.Builtins
import Lam.Eval
import Lam.Json (toAeson)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
stdlibBuiltins :: [Builtin]
stdlibBuiltins = stringBuiltins <> sequenceBuiltins <> [mapMap, jsonPretty]
-- ANCHOR_END: table

-- ANCHOR: strings
-- ✂️ String: what scripts ask of a string every day. The text to look
--    for comes first and the string last, so that |> can be used:
--      line |> String.trim |> String.split ","
-- String：スクリプトが毎日、文字列に頼むこと。探す文字が先、文字列が
-- 最後なので、|> でつなげる
stringBuiltins :: [Builtin]
stringBuiltins =
  [ prim1 "String.trim" (tString ~> tString) (onText T.strip)
  , prim1 "String.trimStart" (tString ~> tString) (onText T.stripStart)
  , prim1 "String.trimEnd" (tString ~> tString) (onText T.stripEnd)
  -- String.split "," "a,,b" is ["a", "", "b"]. An empty separator
  -- cuts between every letter.
  -- 区切りが空なら、1文字ずつに切る
  , prim2 "String.split" (tString ~> tString ~> tList tString) $
      \sep v -> do
        s <- string sep
        t <- string v
        Right (VList (map VString (splitOn s t)))
  , test2 "String.startsWith" T.isPrefixOf
  , test2 "String.endsWith" T.isSuffixOf
  , test2 "String.contains" T.isInfixOf
  -- String.replace "cat" "dog" s: every "cat" becomes "dog". No regex.
  -- 正規表現なしで、全部を置き換える
  , prim3 "String.replace" (tString ~> tString ~> tString ~> tString) $
      \old new v -> do
        o <- string old
        n <- string new
        t <- string v
        Right (VString (T.intercalate n (splitOn o t)))
  , prim2 "String.repeat" (TInt ~> tString ~> tString) $ \n v -> do
      k <- int n
      t <- string v
      Right (VString (T.replicate (max 0 k) t))
  ]
  where
    onText g v = VString . g <$> string v
    test2 n g = prim2 n (tString ~> tString ~> TBool) $ \x v -> do
      needle <- string x
      t      <- string v
      Right (VBool (g needle t))

-- ✂️ Cut at every sep. T.splitOn stops the program on an empty sep, so
--    an empty sep cuts between the letters instead.
-- sep のところで全部切る。T.splitOn は空の sep でプログラムを止めるので、
-- 空なら1文字ずつに切る
splitOn :: Text -> Text -> [Text]
splitOn sep t
  | T.null sep = T.chunksOf 1 t
  | otherwise  = T.splitOn sep t
-- ANCHOR_END: strings

-- ANCHOR: sequences
-- 🔤 The string half of the class Sequence (the prelude's
--    instance Sequence String calls these). Data.Text does the work.
-- クラス Sequence の文字列の側（プレリュードの instance Sequence String
-- が呼ぶ）。仕事は Data.Text がする
sequenceBuiltins :: [Builtin]
sequenceBuiltins =
  [ prim1 "String.reverse" (tString ~> tString) (onText T.reverse)
  , prim2 "String.take" (TInt ~> tString ~> tString) $ \n v -> do
      k <- int n
      VString . T.take k <$> string v
  , prim2 "String.drop" (TInt ~> tString ~> tString) $ \n v -> do
      k <- int n
      VString . T.drop k <$> string v
  ]
  where
    onText g v = VString . g <$> string v

-- 🔢 An Int of Lam, as a Haskell Int (a take of more than that is
--    the same as all of it).
-- Lam の Int を、Haskell の Int に
int :: Value -> Either EvalError Int
int v = case v of
  VInt n -> Right (fromInteger (max (-big) (min big n)))
  _      -> wrong "Int"
  where
    big = toInteger (maxBound :: Int)
-- ANCHOR_END: sequences

-- ANCHOR: mapMap
-- 🗺️ Change every value of a map, and keep its keys. It never compares
--    keys, so it needs no KeyComparable k (the same as Haskell's
--    Map.map). It calls a Lam function, with apply of the evaluator.
-- Map の値を全部変え、キーはそのまま。キーを比べないので、
-- KeyComparable k は要らない（Haskell の Map.map と同じ）。
-- 評価器の apply で、Lam の関数を呼ぶ
mapMap :: Builtin
mapMap =
  prim2 "Map.map" ((a ~> b) ~> tMap k a ~> tMap k b) $ \g v -> do
    m <- mapOf v
    VMap <$> traverse (apply g) m
  where
    k = TVar 0
    a = TVar 1
    b = TVar 2
-- ANCHOR_END: mapMap

-- ANCHOR: pretty
-- 🎀 Json.pretty writes JSON over many lines, with 2 spaces of indent.
--    aeson-pretty does the work.
-- Json.pretty は、JSON を2つの空白で字下げして、何行にも分けて書く。
-- 仕事は aeson-pretty がする
jsonPretty :: Builtin
jsonPretty =
  prim1 "Json.pretty" (TCon "Json" [] ~> tString) $ \v -> do
    j <- toAeson v
    let config = defConfig { confIndent = Spaces 2 }
    Right (VString
      (TL.toStrict (toLazyText (encodePrettyToTextBuilder' config j))))
-- ANCHOR_END: pretty
-- ANCHOR_END: all
