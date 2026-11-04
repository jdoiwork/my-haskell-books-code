-- | This file must NOT compile: without ApplicativeDo, do needs Monad.
-- このファイルはコンパイルできないはず：
-- ApplicativeDo なしの do は Monad が要る
module NoAdo where
import Control.Applicative (ZipList (..))

-- ANCHOR: noado
sums :: [Int]
sums = getZipList $ do
  a <- ZipList [1, 2, 3]
  b <- ZipList [10, 20, 30]
  pure (a + b)
-- ANCHOR_END: noado
