-- ANCHOR: all
-- | Training: keep the last few daily backups. The names carry the date
--   (backup-2026-11-09.db), so sorting the names sorts the days.
-- トレーニング：毎日のバックアップを、新しいものからいくつかだけ残す。
-- 名前に日付がある（backup-2026-11-09.db）ので、名前を並べれば日付順になる
module ShelfAnswers.Rotate (toRemove) where

import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Ord (Down (..))

-- 🧹 The backups to remove: all but the newest n. Other files are left
--    alone.
-- 消すバックアップ：新しい n 個のほかの全部。ほかのファイルには触らない
toRemove :: Int -> [FilePath] -> [FilePath]
toRemove n files = drop n (sortOn Down backups)
  where
    backups = [ f | f <- files, "backup-" `isPrefixOf` f, ".db" `isSuffixOf` f ]
-- ANCHOR_END: all
