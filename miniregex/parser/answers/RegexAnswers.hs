-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   parser chapter's.
-- トレーニングの解答：設計の章のものと、パーサーの章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , groupNames
  , simplify
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T

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

-- The parser chapter's training
-- パーサーの章のトレーニング

-- ANCHOR: names
-- 📛 The names of the named groups, from left to right (a group in a
--    group comes after the group around it).
-- 名前のグループの名前を、左から順に（グループの中のグループは、
-- 外のグループのあと）
groupNames :: Pattern -> [Text]
groupNames (Pattern branches) = concatMap branch branches
  where
    branch (Branch ps) = concatMap piece ps
    piece (Anchor _)                         = []
    piece (Quantified (Group kind inner) _) = here kind ++ groupNames inner
    piece (Quantified _ _)                   = []
    here (Named n) = [n]
    here _         = []
-- ANCHOR_END: names

-- ANCHOR: simplify
-- 🧹 A smaller tree that finds the same: (?:x) with one piece in it is
--    that piece, [b] is b, and the inside is made smaller first.
-- 同じものを見つける、小さな木：中に部品が1つだけの (?:x) はその部品、
-- [b] は b。中から先に小さくする
simplify :: Pattern -> Pattern
simplify (Pattern branches) = Pattern (fmap branch branches)
  where
    branch (Branch ps) = Branch (map piece ps)
    piece (Quantified a q) = unwrap (atom a) q
    piece p                = p

    atom (Group kind inner) = Group kind (simplify inner)
    atom (Class (CharClass False (Single c :| []))) = Lit c
    atom a = a

    -- (?:x) once is x; (?:x) q is x q when x is not repeated itself.
    -- (?:x) を1回なら x。x がくり返しでなければ、(?:x) q は x q
    unwrap (Group NonCapturing (Pattern (Branch [p] :| []))) q =
      case p of
        Quantified inner q'
          | q == once  -> Quantified inner q'
          | q' == once -> Quantified inner q
        _ -> Quantified (Group NonCapturing (single p)) q
    unwrap a q = Quantified a q

    single p = Pattern (Branch [p] :| [])
-- ANCHOR_END: simplify
-- ANCHOR_END: all
