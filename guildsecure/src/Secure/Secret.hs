-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}

-- 🤫 Secrets that must not leak into logs.
-- ログにもれてはいけない秘密
module Secure.Secret
  ( Password
  , mkPassword
  , revealPassword
  , Login (..)
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text  (Text)

import Secure.Username
-- ANCHOR_END: imports

-- ANCHOR: password
-- 🤫 A password. show never prints the inside.
-- パスワード。show は中身を決して出さない
newtype Password = Password Text

instance Show Password where
  show _ = "<redacted>"

-- 🤫 Wrap a password (from a form, for example).
-- パスワードを包む（フォームから来たものなど）
mkPassword :: Text -> Password
mkPassword = Password

-- 🔓 Take the inside out. The name says "I know what I'm doing".
-- 中身を取り出す。名前が「わかってやっています」と言っている
revealPassword :: Password -> Text
revealPassword (Password t) = t
-- ANCHOR_END: password

-- ANCHOR: login
-- 🔑 A login request.
-- ログインの要求
data Login = Login
  { user     :: Username
  , password :: Password
  }
  deriving Show
-- ANCHOR_END: login

-- 📥 Read a login from JSON. The name is parsed at the door.
--    (Password has no ToJSON, so a Login can not be turned back into JSON.)
-- JSON からログインを読む。名前は門でパースする
-- （Password には ToJSON がないので、Login を JSON に戻すことはできない）
instance FromJSON Login where
  parseJSON = withObject "Login" $ \o -> do
    rawName <- o .: "user"
    name    <- either (fail . show) pure (parseUsername rawName)
    pass    <- o .: "password"
    pure (Login name (mkPassword pass))
-- ANCHOR_END: all
