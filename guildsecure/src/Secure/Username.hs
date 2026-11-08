-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- 🪪 Usernames that came from outside: check them once, at the door.
-- 外から来た名前：門で一度だけ確かめる
module Secure.Username
  ( Username
  , UsernameError (..)
  , isValidUsername
  , parseUsername
  , usernameText
  ) where

import Data.Aeson (ToJSON (..))
import Data.Char  (isAsciiLower, isDigit)
import Data.Text  (Text)
import Data.Text  qualified as T
-- ANCHOR_END: imports

-- ANCHOR: validate
-- ❓ Validate: only says yes or no. The Text stays a plain Text.
-- 検証する：はい・いいえを答えるだけ。Text は、ただの Text のまま
isValidUsername :: Text -> Bool
isValidUsername raw =
  T.length raw >= 3 && T.length raw <= 16 && T.all allowed raw
-- ANCHOR_END: validate

-- ANCHOR: parse
-- 🪪 A username: 3 to 16 characters of a-z, 0-9 and _.
--    The constructor is not exported, so the only way to make one
--    is parseUsername.
-- 名前：a〜z、0〜9、_ の3〜16文字。コンストラクターは公開しないので、
-- 作る方法は parseUsername だけ
newtype Username = Username Text
  deriving (Show, Eq, Ord)

-- 🚫 Why a name was turned away.
-- 名前を断った理由
data UsernameError
  = TooShort
  | TooLong
  | BadChar Char
  deriving (Show, Eq)

-- 🪪 Parse: check, and give back a new type that remembers the check.
-- パースする：確かめて、確かめたことを覚えている新しい型で返す
parseUsername :: Text -> Either UsernameError Username
parseUsername raw
  | T.length raw < 3  = Left TooShort
  | T.length raw > 16 = Left TooLong
  | otherwise =
      case T.find (not . allowed) raw of
        Just c  -> Left (BadChar c)
        Nothing -> Right (Username raw)

-- 🔤 The text inside (for showing it, or for saving it).
-- 中の文字列（表示や保存のために）
usernameText :: Username -> Text
usernameText (Username t) = t
-- ANCHOR_END: parse

-- 📤 A name may be written out as JSON (it is not a secret).
-- 名前は JSON に書き出してよい（秘密ではない）
instance ToJSON Username where
  toJSON = toJSON . usernameText

allowed :: Char -> Bool
allowed c = isAsciiLower c || isDigit c || c == '_'
-- ANCHOR_END: all
