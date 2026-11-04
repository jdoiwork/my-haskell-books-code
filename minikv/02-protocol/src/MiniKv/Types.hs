-- ANCHOR: all
-- | The types of minikv: what we store, what we are asked, what we answer.
-- minikv の型：しまうもの、頼まれること、答えるもの
module MiniKv.Types
  ( Key
  , Value (..)
  , Kind (..)
  , kindOf
  , Command (..)
  , Reply (..)
  , KvError (..)
  ) where

import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)

-- ANCHOR: value
-- 🔑 Keys are text.
-- キーは文字列
type Key = Text

-- 📦 A value has exactly one of four kinds. The compiler knows them all,
--    so every command must say what to do with each.
-- 値は、4つの種類のどれか1つ。コンパイラーが全部を知っているので、
-- どのコマンドも、それぞれの種類をどう扱うかを書かないといけない
data Value
  = Str  Text              -- ^ a string (or a number written as one) / 文字列
  | List (Seq Text)        -- ^ a list, open at both ends / 両端が開いたリスト
  | Members (Set Text)     -- ^ a set, no duplicates / 重なりのない集合
  | Hash (Map Text Text)   -- ^ fields and their values / フィールドと値
  deriving (Show, Eq)

-- 🏷️ Just the kind of a value, for messages and the TYPE command.
-- 値の種類だけ（エラーの文や TYPE コマンドのため）
data Kind = KString | KList | KSet | KHash
  deriving (Show, Eq, Ord)

kindOf :: Value -> Kind
kindOf (Str _)     = KString
kindOf (List _)    = KList
kindOf (Members _) = KSet
kindOf (Hash _)    = KHash
-- ANCHOR_END: value

-- ANCHOR: command
-- 📮 Everything a client can ask.
-- クライアントが頼めること、全部
data Command
  = Get Key
  | Set Key Text
  | Del [Key]
  | Exists Key
  | Incr Key
  | IncrBy Key Int
  | LPush Key [Text]
  | RPush Key [Text]
  | LPop Key
  | RPop Key
  | LRange Key Int Int
  | LLen Key
  | SAdd Key [Text]
  | SRem Key [Text]
  | SMembers Key
  | SIsMember Key Text
  | HSet Key Text Text
  | HGet Key Text
  | HGetAll Key
  | TypeOf Key
  | Keys
  deriving (Show, Eq)
-- ANCHOR_END: command

-- ANCHOR: reply
-- 📬 Every answer. Errors are values too, not exceptions.
-- 答えの全部。エラーも、例外ではなく値
data Reply
  = Ok                 -- ^ done / できた
  | Nil                -- ^ nothing there / 何もない
  | Bulk Text          -- ^ one text / 文字列1つ
  | Number Int         -- ^ one number / 数1つ
  | Many [Text]        -- ^ several texts / 文字列いくつか
  | Failed KvError     -- ^ it could not be done / できなかった
  deriving (Show, Eq)

-- ❌ What can go wrong, as a type (not as a string).
-- うまくいかない理由を、文字列ではなく型で
data KvError
  = WrongType Key Kind Kind    -- ^ key, wanted, found / キー、欲しい、実際
  | NotAnInteger Key
  | Overflow Key
  deriving (Show, Eq)
-- ANCHOR_END: reply
-- ANCHOR_END: all
