-- ANCHOR: all
-- | The three ways of handing out letters, as plain functions.
-- 3つの配り方を、ふつうの関数で
-- ANCHOR: header
module MiniMq.Routing
  ( matches
  , topicMatch
  , route
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (tails)
import qualified Data.Text as T
import Data.Text (Text)

import MiniMq.Types
-- ANCHOR_END: header

-- ANCHOR: matches
-- 🔀 Does a rule's key accept a letter's key? One line per way.
-- 決まりの札は、手紙の札を受け入れるか。配り方ごとに1行
matches :: ExchangeKind -> RoutingKey -> RoutingKey -> Bool
matches Fanout _       _   = True
matches Direct pattern key = pattern == key
matches Topic  pattern key = topicMatch (wordsOf pattern) (wordsOf key)

-- ✂️ "quest.dragon.north" → ["quest", "dragon", "north"]
wordsOf :: RoutingKey -> [Text]
wordsOf (RoutingKey key) = T.splitOn "." key
-- ANCHOR_END: matches

-- ANCHOR: topic
-- 🧩 Match the words of a pattern against the words of a key.
--    "*" takes exactly one word; "#" takes any number, even none,
--    so we try every way of letting it take some.
-- 模様の単語を、札の単語と合わせる。
-- 「*」はちょうど1語、「#」は何語でも（0語でも）。だから「#」には、
-- 取れる長さを全部試させる
topicMatch :: [Text] -> [Text] -> Bool
topicMatch []         []       = True
topicMatch ("#" : ps) ws       = any (topicMatch ps) (tails ws)
topicMatch ("*" : ps) (_ : ws) = topicMatch ps ws
topicMatch (p   : ps) (w : ws) = p == w && topicMatch ps ws
topicMatch _          _        = False
-- ANCHOR_END: topic

-- ANCHOR: route
-- 📬 Where does a desk send this letter? Each queue at most once.
-- この仕分け台は、手紙をどこへ送るか（同じキューへは1回だけ）
route :: Exchange -> RoutingKey -> [QueueName]
route exchange key =
  nubOrd [ target rule
         | rule <- rules exchange
         , matches (kind exchange) (bindingKey rule) key ]
-- ANCHOR_END: route
-- ANCHOR_END: all
