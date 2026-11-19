-- ANCHOR: head
-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | A big, made-up library to measure with. Always the same documents.
-- 測るための、大きな作りものの書庫。いつも同じ文書になる。
module MiniSearch.Corpus
  ( corpus
  , wordAt
  , queries
  ) where

import Data.Bits (shiftR)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import MiniSearch.Types
-- ANCHOR_END: head

-- ANCHOR: words
-- 🔤 Word number i, made of syllables: 0 -> "ri", 16 -> "riri", ...
-- i 番目の単語を、音節を並べて作る
wordAt :: Int -> Text
wordAt i = T.concat (map (syllables !!) (digits (i + 1)))
  where
    syllables = [ "ka", "ri", "mo", "ta", "ne", "su", "lo", "pa"
                , "gi", "ru", "me", "to", "sa", "ki", "no", "ha" ]
    digits 0 = []
    digits n = n `mod` 16 : digits (n `div` 16)
-- ANCHOR_END: words

-- ANCHOR: random
-- 🎲 A tiny random number generator (the same numbers every time).
-- 小さな乱数（毎回、同じ数が出る）
nextSeed :: Word64 -> Word64
nextSeed x = x * 6364136223846793005 + 1442695040888963407

-- A number from 0 up to (not including) 1.
-- 0 以上 1 未満の数
unit :: Word64 -> Double
unit x = fromIntegral (x `shiftR` 11) / 9007199254740992

-- 📚 Pick a word: small numbers are common, big ones are rare,
--    like real words ("the" is common, "phoenix" is rare).
-- 単語を選ぶ：小さい番号はよく出て、大きい番号はまれ。
-- 本物の単語と同じ（"the" はよく出て、"phoenix" はまれ）
pickWord :: Int -> Word64 -> Int
pickWord vocab seed =
  let u = unit seed
  in min (vocab - 1) (floor (fromIntegral vocab ** u) - 1)
-- ANCHOR_END: random

-- ANCHOR: corpus
-- 🏭 n documents of len words each, from a vocabulary of 20000 words.
-- 語彙 20000 語から、len 語の文書を n 個作る
corpus :: Int -> Int -> [Document]
corpus n len = go 1 42
  where
    go i seed
      | i > n     = []
      | otherwise =
          let seeds = take len (drop 1 (iterate nextSeed seed))
              body  = T.unwords (map (wordAt . pickWord 20000) seeds)
              doc   = Document (DocId i) (T.pack ("Doc " <> show i)) body
          in doc : go (i + 1) (last seeds)

-- 🔍 Some two-word queries made the same way.
-- 同じように作った、2語の問い合わせ
queries :: Int -> [Text]
queries n =
  let seeds = take (2 * n) (drop 1 (iterate nextSeed 7))
      ws    = map (wordAt . pickWord 2000) seeds
  in pairUp ws
  where
    pairUp (a : b : rest) = (a <> " " <> b) : pairUp rest
    pairUp _              = []
-- ANCHOR_END: corpus
-- ANCHOR_END: all
