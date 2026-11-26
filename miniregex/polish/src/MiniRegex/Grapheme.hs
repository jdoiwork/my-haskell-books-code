-- ANCHOR: all
-- ANCHOR: header
-- | \X: one extended grapheme cluster, what a person sees as one
--   character (UAX #29, Unicode 17.0). 👨‍👩‍👧 is five code points (three
--   people and two ZWJs), 🇯🇵 is two (two regional indicators), and 👍🏽
--   is two (a thumb and a skin tone); each is one cluster.
-- \X：拡張書記素クラスター1つ。人が1文字と見るもの（UAX #29、
-- Unicode 17.0）。👨‍👩‍👧 はコードポイント5つ（3人と ZWJ 2つ）、🇯🇵 は2つ
-- （地域指示子2つ）、👍🏽 は2つ（親指と肌の色）。どれもクラスター1つ
module MiniRegex.Grapheme
  ( clusterEnd
  , clusterEndBy
  , Seen (..)
  , Pict (..)
  , Conj (..)
  , breaksBefore
  , clusters
  , clusterCount
  , takeClusters
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.Text (Text)

import MiniRegex.Cursor (Cursor, back, before, headOf, step, taken)
import MiniRegex.Ucd
  ( Conjunct (..), GraphemeBreak (..), conjunct, graphemeBreak
  , isExtendedPictographic )
-- ANCHOR_END: imports

-- ANCHOR: state
-- 🧠 What the rules need to remember about the cluster so far.
-- 規則が、ここまでのクラスターについて覚えておくこと
data Seen = Seen
  { lastBreak   :: !GraphemeBreak  -- the value of the last character
  , pictJoin    :: !Pict           -- for GB11: 😀 Extend* ZWJ
  , conjunctRun :: !Conj           -- for GB9c: क ् ...
  , ris         :: !Int            -- for GB12/13: regional indicators
                                   --   just before, in a row
  }

-- 🖼️ GB11: an Extended_Pictographic, then Extend*, and then a ZWJ.
-- GB11：Extended_Pictographic のあとに Extend* があり、それから ZWJ
data Pict = NoPict | Pict | PictZwj
  deriving Eq

-- 🪢 GB9c: a consonant, then Extend or Linker, with a Linker or not.
-- GB9c：子音のあとに Extend か Linker。Linker があったか
data Conj = NoConj | Cons | ConsLinker
  deriving Eq
-- ANCHOR_END: state

-- ANCHOR: rules
-- ✂️ Is there a break between the cluster so far and the next
--    character? The rules of UAX #29, in their order.
-- ここまでのクラスターと、次の文字のあいだで切れるか。UAX #29 の規則を、
-- その順に
breaksBefore :: Seen -> Char -> Bool
breaksBefore seen c = case (p, n) of
  (CR, LF)                                   -> False  -- GB3
  _ | p `elem` [Control, CR, LF]             -> True   -- GB4
    | n `elem` [Control, CR, LF]             -> True   -- GB5
  (L, _) | n `elem` [L, V, LV, LVT]          -> False  -- GB6
  _ | p `elem` [LV, V], n `elem` [V, T]      -> False  -- GB7
    | p `elem` [LVT, T], n == T              -> False  -- GB8
    | n `elem` [Extend, ZWJ]                 -> False  -- GB9
    | n == SpacingMark                       -> False  -- GB9a
    | p == Prepend                           -> False  -- GB9b
    | conjunctRun seen == ConsLinker
    , conjunct c == Consonant                -> False  -- GB9c
    | pictJoin seen == PictZwj
    , isExtendedPictographic c               -> False  -- GB11
    | p == RegionalIndicator
    , n == RegionalIndicator, odd (ris seen) -> False  -- GB12, GB13
    | otherwise                              -> True   -- GB999
  where
    p = lastBreak seen
    n = graphemeBreak c
-- ANCHOR_END: rules

-- ANCHOR: remember
-- 📝 Take in one more character.
-- 文字をもう1つ取り入れる
remember :: Seen -> Char -> Seen
remember seen c = Seen n pict conj riCount
  where
    n = graphemeBreak c
    pict
      | isExtendedPictographic c                     = Pict
      | n == Extend, pictJoin seen == Pict           = Pict
      | n == ZWJ, pictJoin seen == Pict              = PictZwj
      | otherwise                                    = NoPict
    conj = case conjunct c of
      Consonant                           -> Cons
      Linker | conjunctRun seen /= NoConj -> ConsLinker
      ConjunctExtend                      -> conjunctRun seen
      _                                   -> NoConj
    riCount
      | n == RegionalIndicator = ris seen + 1
      | otherwise              = 0

-- 🌱 The first character of a cluster: as if the text started here.
-- クラスターの最初の文字：文字列がここから始まるかのように
firstSeen :: Char -> Seen
firstSeen = remember (Seen Control NoPict NoConj 0)
-- ANCHOR_END: remember

-- ANCHOR: end
-- 🧩 Where the cluster that starts here ends. Nothing at the end of the
--    text. One character is always taken, whatever it is.
-- ここから始まるクラスターが、どこで終わるか。文字列の終わりなら
-- Nothing。最初の1文字は、何であっても必ず取る
clusterEnd :: Cursor -> Maybe Cursor
clusterEnd = clusterEndBy breaksBefore

-- 🧩 The same, with other rules for where to break (the tests give it
--    the rules of the real PCRE2, to compare with). A regional
--    indicator pairs with the ones just before the start (GB12, GB13):
--    in 🇯🇵🇫, a \X that starts at 🇵 is 🇵 alone.
-- 同じ。ただし、どこで切るかの規則を渡せる（テストが、くらべるために、
-- 本物の PCRE2 の規則を渡す）。地域指示子は、始まりのすぐ手前のものと
-- 組になる（GB12・GB13）：🇯🇵🇫 の 🇵 から始めた \X は、🇵 だけ
clusterEndBy :: (Seen -> Char -> Bool) -> Cursor -> Maybe Cursor
clusterEndBy breaks cur = endFrom breaks (risBefore cur) cur

-- 🔙 How many regional indicators there are just before, in a row.
-- すぐ手前に、地域指示子がいくつ続いているか
risBefore :: Cursor -> Int
risBefore k = case before k of
  Just c | graphemeBreak c == RegionalIndicator
    -> 1 + maybe 0 risBefore (back 1 k)
  _ -> 0

-- 🧩 The walk itself, given the regional indicators before the start
--    (looked at only when the first character is one).
-- 歩きそのもの。始まりの手前の地域指示子の数をもらう（最初の文字が
-- 地域指示子のときだけ見る）
endFrom :: (Seen -> Char -> Bool) -> Int -> Cursor -> Maybe Cursor
endFrom breaks risBack cur = case step cur of
  Nothing      -> Nothing
  Just (c, k)  -> Just (go (startOf c) k)
  where
    startOf c
      | graphemeBreak c == RegionalIndicator
                  = remember (Seen Control NoPict NoConj risBack) c
      | otherwise = firstSeen c
    go seen k = case step k of
      Just (c, k') | not (breaks seen c) -> go (remember seen c) k'
      _                                  -> k
-- ANCHOR_END: end

-- ANCHOR: helpers
-- 🔪 The text, cut into clusters.
-- 文字列を、クラスターに切る
clusters :: Text -> [Text]
clusters t = go (headOf t)
  where
    -- from the end of a cluster, the regional indicators before are
    -- even: none to count (a long row of flags stays linear)
    -- クラスターの終わりからなら、手前の地域指示子は偶数。数えなくてよい
    -- （国旗が長く続いても、線形のまま）
    go k = case endFrom breaksBefore 0 k of
      Nothing -> []
      Just k' -> taken k k' : go k'

-- 🔢 How many characters a person sees.
-- 人が見る文字の数
clusterCount :: Text -> Int
clusterCount = length . clusters

-- ✂️ The first n clusters: never cuts a family or a flag in half
--    (none for n of 0 or less, as T.take).
-- 最初の n 個のクラスター。家族や国旗を半分に切らない（n が 0 以下なら
-- 空。T.take と同じ）
takeClusters :: Int -> Text -> Text
takeClusters n t = go (max 0 n) (headOf t)
  where
    go 0 k = taken (headOf t) k
    go i k = maybe (taken (headOf t) k) (go (i - 1))
                   (endFrom breaksBefore 0 k)
-- ANCHOR_END: helpers
-- ANCHOR_END: all
