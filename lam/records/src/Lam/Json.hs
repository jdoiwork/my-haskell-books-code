-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | JSON for Lam: aeson reads and writes it, and the values are turned
--   into the Json of the prelude (data Json = JNull | ...), and back.
-- Lam の JSON：読み書きは aeson がして、その値を、プレリュードの
-- Json（data Json = JNull | ...）の値に直す。逆向きも。
module Lam.Json
  ( jsonBuiltins
  , fromAeson
  , toAeson
  , fromScientific
  , parseJson
  ) where

import Control.Applicative ((<|>))

-- JSON itself, and its parser
-- JSON そのものと、そのパーサー
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Parser (jsonLast)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Encode.Pretty
  (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Attoparsec.ByteString.Char8 as P

-- Exact decimal numbers, and the other containers
-- ぴったりの小数と、ほかの入れ物
import qualified Data.Map as Map
import Data.Scientific
  (Scientific, base10Exponent, coefficient, normalize, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
jsonBuiltins :: [Builtin]
jsonBuiltins =
  [ prim1 "Json.parse" (tString ~> tResult tString tJson) $ \v -> do
      text <- string v
      Right $ case parseJson text of
        Right j  -> con "Ok" [fromAeson j]
        Left msg -> con "Err" [VString (T.pack msg)]
  , prim1 "Json.show" (tJson ~> tString) $ \v -> do
      j <- toAeson v
      Right (VString (TL.toStrict (encodeToLazyText j)))
  -- ANCHOR: pretty
  -- The same JSON, on many lines, indented by 2 (aeson-pretty).
  -- 同じ JSON を、何行かに分けて、2つずつ字下げして（aeson-pretty）
  , prim1 "Json.pretty" (tJson ~> tString) $ \v -> do
      j <- toAeson v
      let indented = defConfig { confIndent = Spaces 2 }
      Right (VString (TL.toStrict (TB.toLazyText
                        (encodePrettyToTextBuilder' indented j))))
  -- ANCHOR_END: pretty
  ]
  where
    tJson = TCon "Json" []
    tResult e a = TCon "Result" [e, a]
-- ANCHOR_END: table

-- ANCHOR: parseJson
-- 📖 Read JSON text. When an object has the same key twice, the LAST
--    one wins, as in Lam's {"a": 1, "a": 2} (and Haskell's Map.fromList).
--    aeson's eitherDecodeStrictText keeps the first one, so we use the
--    parser jsonLast of attoparsec-aeson instead.
-- JSON の文字列を読む。オブジェクトに同じキーが2つあれば、Lam の
-- {"a": 1, "a": 2}（と Haskell の Map.fromList）と同じく、後のほうが
-- 勝つ。aeson の eitherDecodeStrictText は最初を残すので、かわりに
-- attoparsec-aeson のパーサー jsonLast を使う
parseJson :: Text -> Either String A.Value
parseJson text = P.parseOnly whole (TE.encodeUtf8 text)
  where
    -- The value, with spaces around it, and nothing after it.
    -- 値と、まわりの空白。後ろには何もない
    whole = P.skipSpace *> jsonLast <* P.skipSpace <* end
    -- 「JSON の値の後ろに、よけいな文字があります」
    end = P.endOfInput <|> fail "extra text after the JSON value"
-- ANCHOR_END: parseJson

-- ANCHOR: fromAeson
-- 📥 aeson's value, as Lam's Json.
-- aeson の値を、Lam の Json にする
fromAeson :: A.Value -> Value
fromAeson j = case j of
  A.Null     -> con "JNull" []
  A.Bool b   -> con "JBool" [VBool b]
  A.Number n -> fromScientific n
  A.String s -> con "JStr" [VString s]
  A.Array xs -> con "JArr" [VList (map fromAeson (V.toList xs))]
  A.Object o -> con "JObj" [VMap (Map.fromList
    [ (Key.toText k, fromAeson x) | (k, x) <- KeyMap.toList o ])]
-- ANCHOR_END: fromAeson

-- ANCHOR: fromScientific
-- 🔢 A number of JSON. aeson keeps it as two whole numbers, c and e:
--    c times 10 to the e (19.99 is 1999 and -2). Lam has no fractions, so:
--      a whole number   ->  JInt 42
--      a fraction       ->  JDec 1999 2   (1999 divided by 10 to the 2)
--    A whole number with a very large e (1e1000000000) is not made
--    into its digits: it stays JDec 1 (-1000000000). aeson draws the
--    same line, 1024, when it writes numbers out.
-- JSON の数。aeson は、2つの整数 c と e（c かける 10 の e 乗）で持つ。
-- Lam には小数がないので、整数は JInt、小数は JDec にする。JDec c d は、
-- c を 10 の d 乗で割った数。e がとても大きい整数（1e1000000000）は、
-- 桁を並べずに JDec のままにする。aeson も、書き出すときに同じ 1024 で
-- 分けている
fromScientific :: Scientific -> Value
fromScientific n
  | e >= 0 && e <= 1024 = con "JInt" [VInt (c * 10 ^ e)]
  | otherwise           = con "JDec" [VInt c, VInt (negate (toInteger e))]
  where
    m = normalize n          -- 19.990 is 1999 and -2, not 19990 and -3
    c = coefficient m
    e = base10Exponent m
-- ANCHOR_END: fromScientific

-- ANCHOR: toAeson
-- 📤 Lam's Json, as aeson's value (to write it out).
-- Lam の Json を、aeson の値にする（書き出すため）
toAeson :: Value -> Either EvalError A.Value
toAeson v = case v of
  VCon "JNull" _ []               -> Right A.Null
  VCon "JBool" _ [VBool b]        -> Right (A.Bool b)
  VCon "JInt"  _ [VInt i]         -> Right (A.Number (fromInteger i))
  VCon "JDec"  _ [VInt c, VInt d] -> A.Number <$> decimal c d
  VCon "JStr"  _ [VString s]      -> Right (A.String s)
  VCon "JArr"  _ [VList xs]       ->
    A.Array . V.fromList <$> traverse toAeson xs
  VCon "JObj"  _ [VMap m]         ->
    A.Object . KeyMap.fromList <$> traverse field (Map.toList m)
  _ -> Left (BuiltinFailed "Json.show" "not a Json value")
  where
    field (k, x) = do
      j <- toAeson x
      Right (Key.fromText k, j)
    -- c divided by 10 to the d. The exponent of aeson is an Int.
    -- c を 10 の d 乗で割った数。aeson の指数は Int
    decimal c d
      | abs d <= toInteger (maxBound :: Int) =
          Right (scientific c (negate (fromInteger d)))
      -- 「桁が大きすぎます」
      | otherwise =
          Left (BuiltinFailed "Json.show" "the number is too large")
-- ANCHOR_END: toAeson
-- ANCHOR_END: all
