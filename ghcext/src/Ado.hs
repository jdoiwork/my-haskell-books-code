{-# LANGUAGE ApplicativeDo #-}
-- ANCHOR: all
-- | ApplicativeDo: do for something that is only Applicative.
-- ApplicativeDo：Applicative でしかないものにも do を
module Ado (sums) where

import Control.Applicative (ZipList (..))

-- ANCHOR: ado
-- 🤐 ZipList has no Monad, but with ApplicativeDo, do works.
-- ZipList は Monad ではないが、ApplicativeDo なら do が使える
sums :: [Int]
sums = getZipList $ do
  a <- ZipList [1, 2, 3]
  b <- ZipList [10, 20, 30]
  pure (a + b)
-- ANCHOR_END: ado
-- ANCHOR_END: all
