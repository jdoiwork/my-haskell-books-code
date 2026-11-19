-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on the cursor and the instructions.
-- トレーニングの解答：設計の章のものと、しおりと命令を磨く章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , indexPlaces
  , starThen
  , starThenTries
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Search (indices)
import Data.Text.Unsafe (lengthWord8, takeWord8)

import MiniRegex.Cursor (Cursor, jumpTo, rest, step)
import MiniRegex.Rules (fitsChar)
import MiniRegex.Syntax
-- ANCHOR_END: head

-- The design chapter's training
-- 設計の章のトレーニング

-- 🌈 colou?r: c, o, l, o, then u zero or one time, then r.
-- colou?r：c・o・l・o、u を 0 回か 1 回、そして r
colourTree :: Pattern
colourTree = Pattern (Branch pieces :| [])
  where
    pieces =
      [ Quantified (Lit 'c') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'l') once
      , Quantified (Lit 'o') once
      , Quantified (Lit 'u') optional
      , Quantified (Lit 'r') once
      ]

-- 🍀 (?:ab)+|c: two branches. The left one is a group that does not
--    keep what it found, one time or more.
-- (?:ab)+|c：枝が2本。左は、見つけたものを覚えないグループを1回以上
choiceTree :: Pattern
choiceTree = Pattern (left :| [right])
  where
    left  = Branch [Quantified (Group NonCapturing ab) plus]
    ab    = Pattern (Branch [ Quantified (Lit 'a') once
                            , Quantified (Lit 'b') once ] :| [])
    right = Branch [Quantified (Lit 'c') once]

-- 🛡️ Put \ before every character with a meaning in a pattern, so
--    that the pattern finds the text just as it is.
-- パターンの中で意味を持つ文字の前に \ を付けて、その文字列そのものを
-- 探すパターンにする
escape :: Text -> Text
escape = T.concatMap one
  where
    one c
      | c `elem` special = T.pack ['\\', c]
      | otherwise        = T.singleton c
    special = "\\^$.|?*+()[]{}" :: String

-- The training of the chapter on the cursor and the instructions
-- しおりと命令を磨く章のトレーニング

-- ANCHOR: indexplaces
-- 🔦 The places where t starts, with indices from text's insides. It
--    gives byte places in what is ahead, so the cursor jumps that many
--    bytes, and as many characters as are in the bytes jumped over.
--    But indices does not find places that overlap: after each one
--    found, search again from one character later.
-- t が始まる場所を、text の内側の indices で。indices は、ここから先の
-- バイトの場所を返すので、しおりはそのバイトだけ飛ぶ。文字の数は、
-- 飛び越えたバイトの中の文字を数える。ただし indices は、重なる場所を
-- 見つけない。見つけるたびに、1文字先から探し直す
indexPlaces :: Text -> Cursor -> [Cursor]
indexPlaces t c = case indices t ahead of
  []    -> []
  k : _ ->
    let here = jumpTo (T.length (takeWord8 k ahead)) k c
    in here : maybe [] (indexPlaces t . snd) (step here)
  where
    ahead = rest c
-- ANCHOR_END: indexplaces

-- ANCHOR: starthen
-- 🍡 x*lit at this cursor, the way Python's REPEAT_ONE does it: first
--    take as many x as there are, counting them, and then give them
--    back one at a time, until lit is just ahead. Only where the next
--    character is lit's first one is lit tried at all. Gives the end
--    of the match.
-- このしおりで x*lit を、Python の REPEAT_ONE のやり方で。まず、x を
-- あるだけ食べて数える。そのあと、lit がすぐ先にあるまで、1つずつ
-- 返す。lit を試すのは、次の文字が lit の最初の文字の場所だけ。
-- 一致の終わりを返す
starThen :: Atom -> Text -> Cursor -> Maybe Cursor
starThen x lit c = fst (starThenTries x lit c)

-- 🍡 The same, and how many times lit was tried.
-- 同じもの。lit を何回試したかも
starThenTries :: Atom -> Text -> Cursor -> (Maybe Cursor, Int)
starThenTries x lit c = giveBack (reverse (eaten c)) 0
  where
    -- the cursors after 0, 1, 2 ... x's (the most last)
    -- x を 0 個・1 個・2 個…食べたあとのしおり（いちばん多いのが最後）
    eaten d = d : case step d of
      Just (ch, d') | fitsChar x ch -> eaten d'
      _                             -> []
    giveBack [] tries = (Nothing, tries)
    giveBack (d : ds) tries
      | worth d = case tryLit d of
          Just e  -> (Just e, tries + 1)
          Nothing -> giveBack ds (tries + 1)
      | otherwise = giveBack ds tries
    -- is lit's first character just ahead? (an empty lit: always)
    -- lit の最初の文字が、すぐ先にあるか（lit が空なら、いつでも）
    worth d = case (T.uncons lit, step d) of
      (Nothing, _)               -> True
      (Just (l, _), Just (y, _)) -> l == y
      _                          -> False
    tryLit d
      | lit `T.isPrefixOf` rest d =
          Just (jumpTo (T.length lit) (lengthWord8 lit) d)
      | otherwise = Nothing
-- ANCHOR_END: starthen
-- ANCHOR_END: all
