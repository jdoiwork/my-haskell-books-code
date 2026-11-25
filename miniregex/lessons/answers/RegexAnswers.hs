-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on learning from other lands.
-- トレーニングの解答：設計の章のものと、よその国に学ぶ章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , innerOfOverAsserts
  , withAsserts
  , literalsThroughGroups
  , withGroups
  ) where

import Control.Monad (guard)
import Data.Array ((!))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
import MiniRegex.Prefilter
import MiniRegex.Program
import MiniRegex.Rules (fitsChar)
import MiniRegex.Syntax
import MiniRegex.Types (Regex (..))
import MiniRegex.Unicode (caseVariants)
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

-- The chapter on learning from other lands
-- よその国に学ぶ章

-- ANCHOR: asserts
-- 🧱 innerOf, but an assertion (\b, \B, ^, $ ...; not a look, which
--    begins with a fence) is passed over, as an instruction that takes
--    no character. The VM still checks it at the place we step back to.
-- innerOf。ただし、たしかめるだけの命令（\b・\B・^・$ など。先読みは
-- fence で始まるので、ちがう）も、文字を食べない命令として、とばす。
-- たしかめるのは、戻った場所で VM がする
innerOfOverAsserts :: Program -> Maybe ([Lead], Text)
innerOfOverAsserts prog = leads (skip 0) []
  where
    skip pc = case prog ! pc of
      Open _    -> skip (pc + 1)
      Close _   -> skip (pc + 1)
      SetMark _ -> skip (pc + 1)
      Assert _  -> skip (pc + 1)
      _         -> pc
    leads pc acc = case prog ! pc of
      Take (Lit _) | not (null acc) -> text pc (reverse acc) []
      Take a
        | null acc, TakeAll b <- prog ! skip (pc + 1), a == b
                    -> leads (skip (skip (pc + 1) + 1)) [Run a]
        | otherwise -> leads (skip (pc + 1)) (One a : acc)
      _             -> Nothing
    text pc ls cs = case prog ! pc of
      Take (Lit c) -> text (skip (pc + 1)) ls (c : cs)
      _            -> case (ls, reverse cs) of
        (Run a : _, c : _) | fitsChar a c -> Nothing
        (_, cs')                          -> Just (ls, T.pack cs')

-- 🧱 The regex, with that start if it has one.
-- その始まりがあれば、それにした正規表現
withAsserts :: Regex -> Regex
withAsserts re = case innerOfOverAsserts (program re) of
  -- with a run, an assert makes a later start in the run worth trying
  -- (\b[.a]+@ does not match .a@ from the dot, but does from a): only
  -- leads of one character each are safe
  -- Run があると、assert のせいで、Run の中の右の始まりにも試す値打ちが
  -- 出る（\b[.a]+@ は .a@ に、. からは合わないが、a からは合う）。
  -- 安全なのは、前置きが1文字ずつのものだけ
  Just (ls, t) | isFirstChar (start (prefilter re)), all isOne ls ->
    re { prefilter = (prefilter re) { start = Inner ls t } }
  _ -> re
  where
    isFirstChar (FirstChar _) = True
    isFirstChar _             = False
    isOne (One _) = True
    isOne _       = False
-- ANCHOR_END: asserts

-- ANCHOR: groups
-- 🎁 literalsOf, but a group that keeps nothing, (?: ... ), is looked
--    into, as many times as it is there: (?:Mira|Ruby) sold is two
--    texts. The order is the VM's: the left choices first.
-- literalsOf。ただし、何も覚えないグループ (?: ... ) の中も見る。
-- (?:Mira|Ruby) sold は2つの文字列。順は VM と同じ：左の選び方が先
literalsThroughGroups :: Pattern -> Maybe [Text]
literalsThroughGroups pat = do
  ws <- choices pat
  -- count up to 65 only (28 groups of (?:a|b) would be 2^28 texts)
  -- 数えるのは 65 個まで（(?:a|b) が 28 個なら 2 の 28 乗個）
  guard (length (take 65 ws) <= 64 && not (any T.null ws))
  pure ws
  where
    choices (Pattern bs) = concat <$> traverse branch (NE.toList bs)
    branch (Branch ps) = map T.concat . sequence <$> traverse piece ps
    piece (Quantified a q) | q == once = case a of
      Lit c                    -> Just [T.singleton c]
      NoCase (Lit c)           -> Just (map T.singleton (caseVariants c))
      Group NonCapturing inner -> choices inner
      _                        -> Nothing
    piece _ = Nothing

-- 🎁 The regex, with only texts if the groups let it be so.
-- グループの中まで見て、文字列だけなら、そうした正規表現
withGroups :: Regex -> Regex
withGroups re = case literalsThroughGroups (tree re) of
  Just ws -> re { prefilter = (prefilter re) { start = Literals ws } }
  Nothing -> re
-- ANCHOR_END: groups
-- ANCHOR_END: all
