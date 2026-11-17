{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- ANCHOR: head
-- | The builtins behind f"...": turn a value into text, and line the
--   text up. They are ordinary functions, so a program can call them
--   too (with a width it computes, for example).
-- f"..." の裏で働く組み込みの関数：値を文字にし、文字の桁をそろえる。
-- ふつうの関数なので、プログラムから呼んでもよい（計算した幅で、など）。
module Lam.Format
  ( formatBuiltins
  , display
  , padLeft
  , padRight
  , center
  , groupDigits
  , zeroPad
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 🧰 The rows for the table of builtins.
-- 組み込みの関数の表の行
formatBuiltins :: [Builtin]
formatBuiltins =
  -- A value as text: show is what the REPL shows ("hi" with quotes),
  -- display is for people (a string as it is, without quotes).
  -- 値を文字に。show は REPL が見せる形（"hi" は引用符つき）、
  -- display は人に見せる形（文字列は、引用符なしのそのまま）
  [ requiring [(Showable, 0)] $
      prim1 "show" (a ~> tString) (Right . VString . renderValue)
  , requiring [(Showable, 0)] $
      prim1 "display" (a ~> tString) (Right . VString . display)

  -- Line a string up in a width:  padLeft 5 "ab"  is  "   ab".
  -- 文字列を、幅の中でそろえる
  , prim2 "padLeft"  (TInt ~> tString ~> tString) (withWidth padLeft)
  , prim2 "padRight" (TInt ~> tString ~> tString) (withWidth padRight)
  , prim2 "center"   (TInt ~> tString ~> tString) (withWidth center)

  -- Numbers:  groupDigits 1234567  is  "1,234,567",
  --           zeroPad 5 42  is  "00042".
  -- 数：3桁ごとに区切る、0 で埋める
  , prim1 "groupDigits" (TInt ~> tString) $ \v -> case v of
      VInt n -> Right (VString (groupDigits n))
      _      -> wrongType "groupDigits"
  , prim2 "zeroPad" (TInt ~> TInt ~> tString) $ \w v -> case (w, v) of
      (VInt width, VInt n) -> Right (VString (zeroPad width n))
      _                    -> wrongType "zeroPad"
  ]
  where
    a = TVar 0
    withWidth f w s = case w of
      VInt width -> VString . f width <$> string s
      _          -> wrongType "a width"
    wrongType what =
      Left (BuiltinFailed what "got a value of the wrong type")
-- ANCHOR_END: table

-- ANCHOR: display
-- 🖨️ A value for people: a string as it is, anything else as the REPL
--    shows it. Inside a list, a string keeps its quotes: ["a", "b"].
-- 人に見せる値：文字列はそのまま、ほかは REPL と同じ形。
-- リストの中の文字列は、引用符つきのまま
display :: Value -> Text
display v = case v of
  VString s -> s
  _         -> renderValue v
-- ANCHOR_END: display

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
-- 🔢 1234567 is "1,234,567", and -1234 is "-1,234".
-- 3桁ごとにカンマで区切る
groupDigits :: Integer -> Text
groupDigits n = sign n <> T.intercalate "," (groups (T.pack (show (abs n))))
  where
    -- "1234567" -> ["1", "234", "567"]: cut 3 letters at a time,
    -- from the right.
    -- 右から3文字ずつ切る
    groups ds = reverse (map T.reverse (T.chunksOf 3 (T.reverse ds)))

-- 🔢 Fill with 0 to make w letters, after the sign:
--    zeroPad 5 42 is "00042", zeroPad 5 (0 - 42) is "-0042".
-- 符号のあとを 0 で埋めて、w 文字にする
zeroPad :: Integer -> Integer -> Text
zeroPad w n =
  let digits = T.pack (show (abs n))
      width  = max 0 (w - fromIntegral (T.length (sign n)))
  in sign n <> T.replicate (fromIntegral (missing width digits)) "0" <> digits

sign :: Integer -> Text
sign n = if n < 0 then "-" else ""
-- ANCHOR_END: numbers
-- ANCHOR_END: all
