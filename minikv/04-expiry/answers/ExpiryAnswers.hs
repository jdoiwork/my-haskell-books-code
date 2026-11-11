-- ANCHOR: all
-- | Answers to the training of "Expiry and the clock" (step 3).
-- 「期限切れと時計」のトレーニングの解答（ステップ3）
-- ANCHOR: head
module ExpiryAnswers
  ( DueIndex
  , emptyDue
  , schedule
  , unschedule
  , takeDue
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MiniKv.Types (Key, Millis)
-- ANCHOR_END: head

-- ANCHOR: index
-- 🗓️ The deadlines, in the order of time. byKey is there to find (and
--    remove) the old deadline of a key when it gets a new one.
-- 期限を、時刻の順に。byKey は、キーに新しい期限が付いたとき、
-- 古い期限を見つけて外すため
data DueIndex = DueIndex
  { byTime :: !(Map Millis (Set Key))
  , byKey  :: !(Map Key Millis)
  }
  deriving (Show, Eq)

emptyDue :: DueIndex
emptyDue = DueIndex Map.empty Map.empty

-- 📌 Give the key a deadline (a new one replaces the old one).
-- キーに期限を付ける（新しい期限は、古い期限と入れかわる）
schedule :: Key -> Millis -> DueIndex -> DueIndex
schedule key t index =
  let DueIndex times keys = unschedule key index
  in  DueIndex (Map.insertWith Set.union t (Set.singleton key) times)
               (Map.insert key t keys)

-- 🧽 Take the deadline away (PERSIST, DEL, SET).
-- 期限を外す（PERSIST・DEL・SET のとき）
unschedule :: Key -> DueIndex -> DueIndex
unschedule key index@(DueIndex times keys) = case Map.lookup key keys of
  Nothing -> index
  Just t  ->
    let shrink s = let s' = Set.delete key s
                   in  if Set.null s' then Nothing else Just s'
    in  DueIndex (Map.update shrink t times) (Map.delete key keys)
-- ANCHOR_END: index

-- ANCHOR: takeDue
-- ⏱️ At most n keys whose time has come, the earliest first, taken out
--    of the index. Only the front of the map is looked at: keys whose
--    time has not come are never visited.
-- 時刻が来たキーを、早いものから多くても n 個、索引から取り出す。
-- 見るのは Map の先頭だけ：時刻の来ていないキーは、調べもしない
takeDue :: Int -> Millis -> DueIndex -> ([Key], DueIndex)
takeDue n now index
  | n <= 0    = ([], index)
  | otherwise = case Map.lookupMin (byTime index) of
      Just (t, ks) | t <= now ->
        let key           = Set.findMin ks
            (rest, index') = takeDue (n - 1) now (unschedule key index)
        in  (key : rest, index')
      _ -> ([], index)
-- ANCHOR_END: takeDue
-- ANCHOR_END: all
