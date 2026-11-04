-- ⚠️ This file does NOT compile, on purpose (the security chapter).
-- わざとコンパイルできないファイル（セキュリティの章）
{-# LANGUAGE DeriveAnyClass #-}
module LeakJson where

import Data.Aeson   (ToJSON)
import GHC.Generics (Generic)

import Secure.Secret   (Password)
import Secure.Username (Username)

-- ANCHOR: leak
-- ❌ Try to turn a login into JSON (for a log, say).
-- ログインを JSON にしようとする（ログに出すため、など）
data LoginLog = LoginLog
  { user     :: Username
  , password :: Password
  }
  deriving (Generic, ToJSON)
-- ANCHOR_END: leak
