{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins behind f"...": turn a value into text, and line the
--   text up. They are ordinary functions, so a program can call them
--   too (with a width it computes, for example). display, the one for
--   people, is a class of Lam in the prelude (Display).
-- f"..." の裏で働く組み込みの関数：値を文字にし、文字の桁をそろえる。
-- ふつうの関数なので、プログラムから呼んでもよい（計算した幅で、など）。
-- 人に見せる display は、プレリュードの Lam のクラス（Display）。
module Lam.Format
  ( formatBuiltins
  , padLeft
  , padRight
  , center
  , groupDigits
  , zeroPad
  ) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Pretty (showFloat)
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 🧰 The rows for the table of builtins.
-- 組み込みの関数の表の行
formatBuiltins :: [Builtin]
formatBuiltins =
  -- ANCHOR: show
  -- A value as the REPL shows it ("hi" with quotes), for any type in
  -- Showable (any type but a function).
  -- 値を、REPL が見せる形の文字に（"hi" は引用符つき）。Showable の
  -- 型なら何でも（関数のほかは何でも）
  [ requiring [(Showable, 0)] $
      prim1 "show" (a ~> tString) (Right . VString . renderValue)
  -- A number, Int or Float, as text: showNumber 7 is "7",
  -- showNumber 2.5 is "2.5".
  -- 数（Int か Float）を文字に
  , requiring [(Number, 0)] $
      prim1 "showNumber" (a ~> tString) $ \v -> case v of
        VInt n   -> Right (VString (T.pack (show n)))
        VFloat x -> Right (VString (showFloat x))
        _        -> wrong "showNumber"
  -- ANCHOR_END: show

  -- Line a string up in a width:  padLeft 5 "ab"  is  "   ab".
  -- 文字列を、幅の中でそろえる
  , prim2 "padLeft"  (TInt ~> tString ~> tString) (withWidth padLeft)
  , prim2 "padRight" (TInt ~> tString ~> tString) (withWidth padRight)
  , prim2 "center"   (TInt ~> tString ~> tString) (withWidth center)

  -- The text of a number:  groupDigits "1234567.5"  is  "1,234,567.5",
  --                        zeroPad 5 "-42"  is  "-0042".
  -- 数の文字：3桁ごとに区切る、0 で埋める
  , prim1 "groupDigits" (tString ~> tString) $ \v ->
      VString . groupDigits <$> string v
  , prim2 "zeroPad" (TInt ~> tString ~> tString) (withWidth zeroPad)
  ]
  where
    a = TVar 0
    withWidth f w s = case w of
      VInt width -> VString . f width <$> string s
      _          -> wrong "a width"
-- ANCHOR_END: table

-- ANCHOR: pad
-- 📏 Put spaces on the left, on the right, or on both sides, until the
--    text is w letters long. A longer text is left as it is.
--    center puts the extra space on the right, like Python.
-- 左・右・両側に空白を足して、w 文字にする。長い文字は、そのまま。
-- center は、Python と同じく、割り切れない1つを右に置く
padLeft, padRight, center :: Integer -> Text -> Text
padLeft  w s = spaces (missing w s) <> s
padRight w s = s <> spaces (missing w s)
center   w s =
  let d    = missing w s
      left = d `div` 2
  in spaces left <> s <> spaces (d - left)

-- How many letters are missing to make w.
-- w にするのに、足りない文字の数
missing :: Integer -> Text -> Integer
missing w s = max 0 (w - fromIntegral (T.length s))

spaces :: Integer -> Text
spaces n = T.replicate (fromIntegral n) " "
-- ANCHOR_END: pad

-- ANCHOR: numbers
-- 🔢 A comma every 3 digits, in the digits before the point:
--    "1234567" is "1,234,567", "-1234.5" is "-1,234.5".
-- 小数点より前の数字に、3桁ごとにカンマを入れる
groupDigits :: Text -> Text
groupDigits t =
  let (sign, rest)    = T.span (== '-') t
      (digits, after) = T.span isDigit rest
  in sign <> T.intercalate "," (groups digits) <> after
  where
    -- "1234567" -> ["1", "234", "567"]: cut 3 letters at a time,
    -- from the right.
    -- 右から3文字ずつ切る
    groups ds = reverse (map T.reverse (T.chunksOf 3 (T.reverse ds)))

-- 🔢 Fill with 0 to make w letters, after the sign:
--    zeroPad 5 "42" is "00042", zeroPad 5 "-42" is "-0042".
-- 符号のあとを 0 で埋めて、w 文字にする
zeroPad :: Integer -> Text -> Text
zeroPad w t =
  let (sign, rest) = T.span (== '-') t
  in sign <> T.replicate (fromIntegral (missing w t)) "0" <> rest
-- ANCHOR_END: numbers
-- ANCHOR_END: all
