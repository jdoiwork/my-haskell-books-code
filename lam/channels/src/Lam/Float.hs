-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Floats: turning numbers into each other, rounding, and a few
--   functions of math. The arithmetic itself (+ - * /) is in the
--   evaluator (binOp).
-- Float：数どうしの変換、丸め、いくつかの数学の関数。計算そのもの
-- （+ - * /）は評価器（binOp）にある。
module Lam.Float
  ( floatBuiltins
  , roundAway
  , fixedText
  , readFloat
  ) where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Lam.Builtins
import Lam.Eval
import Lam.Json (fromScientific, toAeson)
import qualified Data.Aeson as A
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 The rows to add to the table of builtins.
-- 組み込みの関数の表に足す行
floatBuiltins :: [Builtin]
floatBuiltins =
  [ -- ANCHOR: toFloat
    -- Int to Float: always works.  toFloat 3 is 3 (a Float).
    -- Int を Float に。いつもできる
    prim1 "toFloat" (TInt ~> tFloat) $ \v -> VFloat <$> number v
    -- ANCHOR_END: toFloat
  -- ANCHOR: rounding
  -- Float to Int: not for NaN and the infinities, so Maybe Int.
  --   round 2.5 is Just 3,  round (0 / 0) is Nothing
  -- Float を Int に。NaN と無限大はできないので Maybe Int
  , toInt "round"    roundAway          -- 2.5 -> 3,  -2.5 -> -3
  , toInt "floor"    floor              -- 2.7 -> 2,  -2.7 -> -3
  , toInt "ceiling"  ceiling            -- 2.1 -> 3,  -2.1 -> -2
  , toInt "truncate" truncate           -- 2.7 -> 2,  -2.7 -> -2
  -- The two other ways to round a half:
  -- 半分の丸め方の、ほかの2つ
  , toInt "Float.roundTiesEven" round   -- 2.5 -> 2,  3.5 -> 4
  , toInt "Float.roundTiesUp"           -- 2.5 -> 3,  -2.5 -> -2
      (\r -> floor (r + 1 / 2))
  -- ANCHOR_END: rounding
  -- ANCHOR: math
  -- -x is negate x, for any Number.
  -- -x は negate x。どの Number にも
  , requiring [(Number, 0)] $ prim1 "negate" (a ~> a) $ \v -> case v of
      VInt n   -> Right (VInt (negate n))
      VFloat x -> Right (VFloat (negate x))
      _        -> wrong "a number"
  , onFloat "sqrt" sqrt
  , onFloat "Float.exp" exp
  , onFloat "Float.log" log             -- the natural log / 自然対数
  , prim2 "Float.pow" (tFloat ~> tFloat ~> tFloat) $ \x y ->
      VFloat <$> ((**) <$> number x <*> number y)
  , Builtin "Float.pi" (closeOver tFloat) (VFloat pi)
  , isFloat "Float.isNaN" isNaN
  , isFloat "Float.isInfinite" isInfinite
  -- ANCHOR_END: math
  -- ANCHOR: fixed
  -- n digits after the point, the half away from 0 (the exact Float):
  -- Float.fixed 1 72.25 is "72.3". f"{x:.1}" calls this.
  -- 小数点以下 n けた。半分は 0 から遠いほうへ（Float のぴったりの値で）
  , requiring [(Number, 0)] $
      prim2 "Float.fixed" (TInt ~> a ~> tString) $ \n v -> do
        digits <- int n
        x      <- number v
        Right (VString (fixedText (fromInteger (max 0 digits)) x))
  -- ANCHOR_END: fixed
  -- ANCHOR: parse
  -- Text to Float:  String.toFloat "1.5" is Just 1.5
  -- 文字を Float に
  , prim1 "String.toFloat" (tString ~> tMaybe tFloat) $ \v -> do
      s <- string v
      Right (maybeValue (VFloat <$> readFloat (T.strip s)))
  -- ANCHOR_END: parse
  -- ANCHOR: json
  -- JSON and Float. A number of JSON (JInt or JDec) gives a Float;
  -- a Float gives the shortest JSON number for it. NaN and the
  -- infinities have no JSON number: null, as JSON.stringify does.
  -- JSON と Float。JSON の数（JInt か JDec）は Float に。Float は、
  -- それを表すいちばん短い JSON の数に。NaN と無限大は null
  , prim1 "Json.toFloat" (tJson ~> tMaybe tFloat) $ \v ->
      Right $ maybeValue $ case toAeson v of
        Right (A.Number s) -> Just (VFloat (toRealFloat s))
        _                  -> Nothing
  , prim1 "Json.fromFloat" (tFloat ~> tJson) $ \v -> do
      x <- number v
      Right $ if isNaN x || isInfinite x
        then con "JNull" []
        else fromScientific (fromFloatDigits x)
  -- ANCHOR_END: json
  ]
  where
    a = TVar 0
    tJson = TCon "Json" []
    tMaybe t = TCon "Maybe" [t]
    -- A Float to an Int, by the rounding f (on the exact value).
    -- Float を Int に。丸め方 f で（ぴったりの値に対して）
    toInt n f = prim1 n (tFloat ~> tMaybe TInt) $ \v -> do
      x <- number v
      Right $ maybeValue $
        if isNaN x || isInfinite x
          then Nothing
          else Just (VInt (f (toRational x)))
    onFloat n f = prim1 n (tFloat ~> tFloat) $ \v -> VFloat . f <$> number v
    isFloat n f = prim1 n (tFloat ~> TBool) $ \v -> VBool . f <$> number v
-- ANCHOR_END: table

-- ANCHOR: helpers
-- 🔍 A number as a Double: a Float, or an Int (a Float written as 3).
-- 数を Double に。Float か、Int（3 と書いた Float）
number :: Value -> Either EvalError Double
number v = maybe (wrong "a number") Right (floatOf v)

-- 🔍 An Int.
-- Int を取り出す
int :: Value -> Either EvalError Integer
int v = case v of
  VInt n -> Right n
  _      -> wrong "a whole number"

-- ANCHOR: roundAway
-- 🎯 Round to the nearest whole number; a half goes away from 0, as in
--    arithmetic at school (and Rust, C, Swift).
-- いちばん近い整数に。半分は 0 から遠いほうへ（算数と同じ）
roundAway :: Rational -> Integer
roundAway r =
  let (whole, part) = properFraction r
  in if abs part >= 1 / 2 then whole + (if r < 0 then -1 else 1) else whole

-- ANCHOR_END: roundAway

-- ANCHOR: fixedText
-- 📏 x with n digits after the point:  fixedText 2 3.14159 is "3.14".
-- 小数点以下 n けたの x
fixedText :: Int -> Double -> Text
fixedText n x
  | isNaN x      = "NaN"
  | isInfinite x = if x > 0 then "Infinity" else "-Infinity"
  | otherwise    =
      let scaled = roundAway (abs (toRational x) * 10 ^ n)
          digits = T.justifyRight (n + 1) '0' (T.pack (show scaled))
          (whole, part) = T.splitAt (T.length digits - n) digits
          sign = if x < 0 then "-" else ""
      in sign <> whole <> (if n == 0 then "" else "." <> part)

-- ANCHOR_END: fixedText

-- ANCHOR: readFloat
-- 📖 Read a Float from text: 1.5, -2, 1e3. Nothing for anything else
--    (NaN and Infinity, too). Haskell's read gives the Float nearest
--    to the number written. (Data.Text.Read's double is faster, but
--    may be off in the last digit: the test of the training found it.)
-- 文字から Float を読む。だめなら Nothing（NaN と Infinity も）。
-- Haskell の read は、書いた数にいちばん近い Float をくれる
-- （Data.Text.Read の double は速いが、最後の桁がずれることがある。
-- トレーニングのテストが見つけた）
readFloat :: Text -> Maybe Double
readFloat s
  | T.all (`elem` ("0123456789+-.e" :: String)) s =
      readMaybe (T.unpack (fromMaybe s (T.stripPrefix "+" s)))
  | otherwise = Nothing
-- ANCHOR_END: readFloat
-- ANCHOR_END: helpers
-- ANCHOR_END: all
