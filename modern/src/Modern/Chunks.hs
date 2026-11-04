-- ANCHOR: all
-- | Splitting a list of quests into parties of n: an answer an AI
--   might give, and the properties that check any answer.
-- 依頼のリストを n 個ずつに分ける：AI が返しそうな答えと、
-- どんな答えでも確かめられる性質
module Modern.Chunks
  ( chunksDraft
  , chunks
  ) where

-- ANCHOR: draft
-- 🤖 A plausible first answer. It looks right, and it passes the
--    example in the request (6 quests in groups of 2).
-- それらしい最初の答え。正しそうに見えるし、頼むときに添えた例
-- （6 個を 2 個ずつ）にも合う
chunksDraft :: Int -> [a] -> [[a]]
chunksDraft n xs
  | length xs < n = []
  | otherwise     = take n xs : chunksDraft n (drop n xs)
-- ANCHOR_END: draft

-- ANCHOR: chunks
-- ✅ Keep the short group at the end, and stop on an empty list.
-- 最後の短い組も残し、空のリストで止まる
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
-- ANCHOR_END: chunks
-- ANCHOR_END: all
