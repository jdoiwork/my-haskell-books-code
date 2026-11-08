-- ANCHOR: all
-- | String algorithms: run-length encoding, searching with KMP, and a trie.
-- 文字列のアルゴリズム：ランレングス符号、KMP で探す、トライ木
module Algo.Strings
  ( runLength
  , unRunLength
  , findAllSpec
  , prefixTable
  , findAll
  , Trie (..)
  , emptyTrie
  , insert
  , fromWords
  , complete
  , monsters
  ) where

-- ANCHOR: imports
import Data.Array
import Data.List (group, isPrefixOf, tails)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
-- ANCHOR_END: imports

-- ANCHOR: rle
-- 🧱 Run-length encoding: each run of the same letter becomes
--    (letter, count).
-- ランレングス符号：同じ文字の並びを（文字, 個数）に
runLength :: String -> [(Char, Int)]
runLength s = [ (c, length run) | run@(c : _) <- group s ]

unRunLength :: [(Char, Int)] -> String
unRunLength runs = concat [ replicate n c | (c, n) <- runs ]
-- ANCHOR_END: rle

-- ANCHOR: spec
-- 📜 The spec: every place where the pattern starts. O(n * m).
-- 仕様：パターンが始まる位置を全部。O(n * m)
findAllSpec :: String -> String -> [Int]
findAllSpec pat text =
  [ i | (i, rest) <- zip [0 ..] (tails text), pat `isPrefixOf` rest ]
-- ANCHOR_END: spec

-- ANCHOR: table
-- 🧮 KMP's table: for each prefix of the pattern, the length of its longest
--    proper border (a beginning that is also an ending). The table is
--    defined from itself: each entry looks at earlier entries.
-- KMP の表：パターンの先頭部分ごとに、いちばん長い「境界」
-- （頭と、お尻が同じになる部分）の長さ。表は、表自身から定義する。
-- どのマスも、前のマスを見る
prefixTable :: String -> Array Int Int
prefixTable pat = table
  where
    m     = length pat
    p     = listArray (0, m - 1) pat
    table = listArray (0, m - 1)
              (0 : [ border i (table ! (i - 1)) | i <- [1 .. m - 1] ])
    border i k
      | p ! i == p ! k = k + 1
      | k == 0         = 0
      | otherwise      = border i (table ! (k - 1))
-- ANCHOR_END: table

-- ANCHOR: kmp
-- 🔎 Walk through the text once, keeping how much of the pattern matches.
--    On a mismatch, fall back using the table instead of starting over.
-- 文を1回だけたどり、パターンのどこまで合っているかを持ち回る。
-- 合わなければ、最初からやり直さずに、表を見て戻る
findAll :: String -> String -> [Int]
-- The empty pattern is found at every position, the end included
--    (just like findAllSpec).
-- 空のパターンは、どの位置にも（最後の位置にも）見つかる
-- （findAllSpec と同じ）
findAll [] text = [0 .. length text]
findAll pat text =
  [ i - m + 1 | (i, k) <- zip [0 ..] (drop 1 (scanl step 0 text)), k == m ]
  where
    m     = length pat
    p     = listArray (0, m - 1) pat
    table = prefixTable pat
    step k c
      | k == m        = step (table ! (m - 1)) c
      | p ! k == c    = k + 1
      | k == 0        = 0
      | otherwise     = step (table ! (k - 1)) c
-- ANCHOR_END: kmp

-- ANCHOR: trie
-- 🌳 A trie: does a word end here, and where each next letter leads.
-- トライ木：ここで終わる単語があるか、次の文字ごとの行き先
data Trie = Trie { endsHere :: Bool, children :: Map Char Trie }
  deriving (Show, Eq)

emptyTrie :: Trie
emptyTrie = Trie False Map.empty

insert :: String -> Trie -> Trie
insert []       t = t { endsHere = True }
insert (c : cs) t =
  let child = Map.findWithDefault emptyTrie c (children t)
  in  t { children = Map.insert c (insert cs child) (children t) }

fromWords :: [String] -> Trie
fromWords = foldr insert emptyTrie

-- 💡 Every word that starts with the prefix, in alphabetical order.
-- 先頭がこれで始まる単語を、全部（辞書の順に）
complete :: String -> Trie -> [String]
complete prefix t = maybe [] (map (prefix ++) . wordsIn) (walk prefix t)
  where
    walk []       node = Just node
    walk (c : cs) node = Map.lookup c (children node) >>= walk cs
    wordsIn node =
      [ "" | endsHere node ]
        ++ [ c : w | (c, child) <- Map.toList (children node)
                   , w <- wordsIn child ]

monsters :: [String]
monsters =
  [ "goblin", "golem", "gorgon", "ghost", "griffin", "slime"
  , "skeleton", "siren", "dragon", "drake" ]
-- ANCHOR_END: trie
-- ANCHOR_END: all
