-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Regexes that know their named groups in their type. Made by the
--   quasi-quoter [typed|…|] of MiniRegex.Quote; a match gives what a
--   group found as a field: m.year. A group that every match sets is a
--   Text; one that may take no part is a Maybe Text.
-- 名前付きのグループを型で知っている正規表現。MiniRegex.Quote の準クォート
-- [typed|…|] が作る。一致したものは、グループが見つけたものをフィールドの
-- ように出す：m.year。どの一致でも必ず使われるグループは Text、使われない
-- かもしれないグループは Maybe Text
module MiniRegex.Typed
  ( Part (..)
  , Groups
  , TypedRegex
  , TypedMatch
  , unsafeTyped
  , Lookup
  , FieldOf
  , Group
  , untyped
  , find
  , findAll
  , whole
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Records (HasField (..))
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import MiniRegex qualified
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🏷️ Does every match set the group, or only some?
-- どの一致でも必ず使われるか、使われないこともあるか
data Part = Always | Sometimes

-- 📇 The named groups of a pattern: [("year", Always), ...].
-- パターンの名前付きのグループ：[("year", Always), ...]
type Groups = [(Symbol, Part)]

-- 🧾 A regex, and its groups in the type.
-- 正規表現と、型に書いたそのグループ
newtype TypedRegex (gs :: Groups) = TypedRegex MiniRegex.Regex

-- 🎯 A match of such a regex. It has no fields of its own, so HasField
--    may give it any field we like.
-- そういう正規表現の一致。自分のフィールドを持たないので、HasField で
-- 好きなフィールドを付けられる
newtype TypedMatch (gs :: Groups) = TypedMatch MiniRegex.Match

-- 🔒 The groups in the type are a promise: only [typed|…|] may make one
--    (the constructors are not exported), and coerce may not change
--    them (nominal: gs must be the same type, not just the same shape
--    underneath).
-- 型に書いたグループは約束：作れるのは [typed|…|] だけ（構成子は export
-- しない）。coerce でも変えられない（nominal：gs は、中身の形が同じ
-- だけでなく、同じ型でなければならない）
type role TypedRegex nominal
type role TypedMatch nominal

-- ⚠️ For the quasi-quoter only: the groups are trusted, not checked.
-- 準クォートのためだけのもの。グループは確かめずに信じる
unsafeTyped :: MiniRegex.Regex -> TypedRegex gs
unsafeTyped = TypedRegex
-- ANCHOR_END: types

-- ANCHOR: family
-- 🔎 Look the name up in the groups, at compile time: Just the part,
--    or Nothing.
-- 名前をグループの中から、コンパイルのときに探す。あれば Just で
-- その使われ方、なければ Nothing
type Lookup :: Symbol -> Groups -> Maybe Part
type family Lookup name gs where
  Lookup name ('(name, p) ': _) = 'Just p
  Lookup name (_ ': rest)       = Lookup name rest
  Lookup name '[]               = 'Nothing

-- 📜 "a, b, c": the names, for the error message.
-- 「a, b, c」：エラーの文に入れる、名前の並び
type Names :: Groups -> ErrorMessage
type family Names gs where
  Names '[]               = 'Text "(none)"
  Names '[ '(n, _)]       = 'Text n
  Names ('(n, _) ': rest) = 'Text n ':<>: 'Text ", " ':<>: Names rest
-- ANCHOR_END: family

-- ANCHOR: field
-- 🧺 The type of the field: Text for a group that is always set, Maybe
--    Text for one that may not be. A name that is not there stops the
--    build, with the names that are.
-- フィールドの型：必ず使われるグループなら Text、使われないかもしれない
-- なら Maybe Text。ない名前は、ある名前を並べて、ビルドを止める
type FieldOf :: Symbol -> Groups -> Maybe Part -> Type
type family FieldOf name gs found where
  FieldOf _    _  ('Just 'Always)    = Text
  FieldOf _    _  ('Just 'Sometimes) = Maybe Text
  FieldOf name gs 'Nothing           = TypeError
    ( 'Text "The pattern has no group named \"" ':<>: 'Text name
        ':<>: 'Text "\"."
      ':$$: 'Text "Its named groups: " ':<>: Names gs )
-- ANCHOR_END: field

-- ANCHOR: instance
-- 🪄 m.year: what the group named "year" found.
-- m.year：year という名前のグループが見つけたもの
instance ( KnownSymbol name
         , found ~ Lookup name gs
         , t ~ FieldOf name gs found
         , Group name gs found )
      => HasField name (TypedMatch gs) t where
  getField (TypedMatch m) =
    fromNamed @name @gs @found (MiniRegex.named key m)
    where
      key = T.pack (symbolVal (Proxy @name))

-- 🧷 From the Maybe Text of named, to the type of the field.
-- named の Maybe Text から、フィールドの型へ
type Group :: Symbol -> Groups -> Maybe Part -> Constraint
class Group name gs found where
  fromNamed :: Maybe Text -> FieldOf name gs found

instance Group name gs ('Just 'Always) where
  fromNamed = fromMaybe (error "an Always group took no part")

instance Group name gs ('Just 'Sometimes) where
  fromNamed = id

-- (never used: the TypeError stops the build first)
-- （使われることはない。その前に TypeError がビルドを止める）
instance Group name gs 'Nothing where
  fromNamed = error "no such group"
-- ANCHOR_END: instance

-- ANCHOR: find
-- 🔓 The plain regex inside, for every other function of MiniRegex.
-- 中の、ふつうの正規表現。MiniRegex のほかの関数のために
untyped :: TypedRegex gs -> MiniRegex.Regex
untyped (TypedRegex r) = r

-- 🔍 The first match, typed.
-- 最初に見つかったもの（型付き）
find :: TypedRegex gs -> Text -> Maybe (TypedMatch gs)
find (TypedRegex r) t = TypedMatch <$> MiniRegex.find r t

-- 🔎 All the matches, typed.
-- 見つかったもの全部（型付き）
findAll :: TypedRegex gs -> Text -> [TypedMatch gs]
findAll (TypedRegex r) t = map TypedMatch (MiniRegex.findAll r t)

-- 🎯 The whole match.
-- 一致の全体
whole :: TypedMatch gs -> Text
whole (TypedMatch m) = MiniRegex.matchText m
-- ANCHOR_END: find
-- ANCHOR_END: all
