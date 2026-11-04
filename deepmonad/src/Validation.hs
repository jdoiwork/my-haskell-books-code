-- ANCHOR: all
-- | An Applicative that collects every error, instead of stopping at
--   the first one like Either.
-- Either のように最初のエラーで止まらず、エラーを全部集める Applicative
module Validation
  ( Validation (..)
  , Applicant (..)
  , checkName
  , checkAge
  , checkJob
  , applyEither
  , applyAll
  , toEither
  ) where

-- ANCHOR: validation
-- 📋 Either all good, or a pile of reasons.
-- 全部よいか、理由の山か
data Validation e a
  = Invalid e
  | Valid a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Invalid e) = Invalid e
  fmap f (Valid a)   = Valid (f a)

-- 🤝 The point: when both sides are Invalid, keep both reasons (<>).
-- ここが肝心：両方が Invalid なら、両方の理由を残す（<>）
instance Semigroup e => Applicative (Validation e) where
  pure = Valid
  Invalid e1 <*> Invalid e2 = Invalid (e1 <> e2)
  Invalid e1 <*> Valid _    = Invalid e1
  Valid f    <*> v          = fmap f v
-- ANCHOR_END: validation

-- ANCHOR: applicant
-- 📝 Someone who wants to join the guild.
-- ギルドに入りたい人
data Applicant = Applicant
  { name :: String
  , age  :: Int
  , job  :: String
  }
  deriving (Show, Eq)

checkName :: String -> Validation [String] String
checkName n
  | null n         = Invalid ["name is empty"]
  | length n > 12  = Invalid ["name is too long"]
  | otherwise      = Valid n

checkAge :: Int -> Validation [String] Int
checkAge a
  | a < 12    = Invalid ["too young"]
  | otherwise = Valid a

checkJob :: String -> Validation [String] String
checkJob j
  | j `elem` ["warrior", "mage", "healer"] = Valid j
  | otherwise                              = Invalid ["unknown job"]
-- ANCHOR_END: applicant

-- ANCHOR: compare
-- 🛑 With Either: stops at the first problem.
-- Either で：最初の問題で止まる
applyEither :: String -> Int -> String -> Either [String] Applicant
applyEither n a j =
  Applicant <$> toEither (checkName n)
            <*> toEither (checkAge a)
            <*> toEither (checkJob j)

-- 📋 With Validation: the same shape, but every problem is reported.
-- Validation で：同じ形なのに、問題が全部わかる
applyAll :: String -> Int -> String -> Validation [String] Applicant
applyAll n a j =
  Applicant <$> checkName n
            <*> checkAge a
            <*> checkJob j

toEither :: Validation e a -> Either e a
toEither (Invalid e) = Left e
toEither (Valid a)   = Right a
-- ANCHOR_END: compare
-- ANCHOR_END: all
