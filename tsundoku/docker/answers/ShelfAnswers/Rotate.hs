-- ANCHOR: all
-- | Training: keep the last few daily backups. The names carry the date
--   (backup-2026-11-09.db), so sorting the names sorts the days.
-- トレーニング：毎日のバックアップを、新しいものからいくつかだけ残す。
-- 名前に日付がある（backup-2026-11-09.db）ので、名前を並べれば日付順になる
module ShelfAnswers.Rotate (toRemove) where

import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Ord (Down (..))

-- 🧹 The backups to remove: all but the newest n. Only the daily ones
--    (backup-YYYY-MM-DD.db) count; others, like backup-before-upgrade.db
--    taken by hand, are left alone. Fewer than one to keep removes
--    nothing: a mistyped number must not empty the shelf of backups.
-- 消すバックアップ：新しい n 個のほかの全部。数えるのは毎日のもの
-- （backup-YYYY-MM-DD.db）だけ。手で取った backup-before-upgrade.db などには
-- 触らない。残す数が1より小さければ、何も消さない（打ちまちがいで、写しが
-- 全部消えないように）
toRemove :: Int -> [FilePath] -> [FilePath]
toRemove n files
  | n < 1     = []
  | otherwise = drop n (sortOn Down daily)
  where
    daily = [ f | f <- files, length f == 20
                , "backup-" `isPrefixOf` f, ".db" `isSuffixOf` f
                , isDay (take 10 (drop 7 f)) ]
    -- "2026-11-09": digits in the shape YYYY-MM-DD
    -- 「2026-11-09」：YYYY-MM-DD の形の数字
    isDay d = and (zipWith fits "dddd-dd-dd" d)
    fits 'd' c = isDigit c
    fits k   c = k == c
-- ANCHOR_END: all
