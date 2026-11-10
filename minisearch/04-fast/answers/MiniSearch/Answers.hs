-- ANCHOR: all
-- ANCHOR: head
-- | Answers to the training of the performance chapter.
-- 速くする章のトレーニングの解答。
module MiniSearch.Answers
  ( topK
  , saveVersioned
  , loadVersioned
  , formatVersion
  ) where

import Control.Exception (IOException, try)
import qualified Data.Binary as Binary
import Data.List (insertBy)
import Data.Ord (Down (..), comparing)

import MiniSearch.Store (replaceFile)
import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: topk
-- 🏆 The k best hits, without sorting them all: keep a short,
--    sorted list of the best so far, and drop the rest.
-- 上位 k 件を、全部を並べずに選ぶ：これまでの上位だけを、
-- 並べた短いリストに持ち、それより下は捨てる
topK :: Int -> [Hit] -> [Hit]
topK k = foldl' keep []
  where
    rank h       = (Down (hitScore h), hitDoc h)
    keep best h  = take k (insertBy (comparing rank) h best)
-- ANCHOR_END: topk

-- ANCHOR: versioned
-- 🏷️ The version of the file format. Change it when Index changes.
-- ファイルの形式の版。Index を変えたら、これも変える
formatVersion :: Int
formatVersion = 4

-- 💾 Write the version first, then the index. Like saveIndex, write
--    a temporary file and swap it in (replaceFile).
-- 先に版を、それからインデックスを書く。saveIndex と同じく、
-- 一時ファイルに書いてから付け替える（replaceFile）
saveVersioned :: FilePath -> Index -> IO ()
saveVersioned path index =
  replaceFile path (Binary.encode (formatVersion, index))

-- 📥 Read the version first; refuse a file of another version.
-- 先に版を読む。違う版のファイルは断る
loadVersioned :: FilePath -> IO (Either String Index)
loadVersioned path = do
  result <- try (Binary.decodeFileOrFail path)
  pure $ case result of
    Left err -> Left (show (err :: IOException))
    Right (Left (_, msg)) -> Left msg
    Right (Right (v, index))
      | v == formatVersion -> Right index
      | otherwise ->
          Left ("this index is version " <> show (v :: Int)
                <> ", but this program reads version "
                <> show formatVersion <> ": run index again")
-- ANCHOR_END: versioned
-- ANCHOR_END: all
