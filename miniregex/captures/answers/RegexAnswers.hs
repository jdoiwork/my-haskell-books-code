-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   groups chapter's.
-- トレーニングの解答：設計の章のものと、グループの章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , splitKeeping
  , namedGroups
  , settings
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex
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

-- The groups chapter's training
-- グループの章のトレーニング

-- ANCHOR: keeping
-- ✂️ split, but with what the groups found between the pieces (as
--    Python's re.split does; a group that found nothing gives "").
-- split と同じ。ただし、切れ端のあいだに、グループが見つけたものも
-- はさむ（Python の re.split と同じ。何も見つけなかったグループは ""）
splitKeeping :: Regex -> Text -> [Text]
splitKeeping re t = go 0 t (findAll re t)
  where
    -- cut from what is left of the text, as split does
    -- split と同じく、文字列の残りから切る
    go _    rest []       = [rest]
    go from rest (m : ms) =
      let (before, atMatch) = T.splitAt (matchStart m - from) rest
          after             = T.drop (matchEnd m - matchStart m) atMatch
      in  before : map (fromMaybe "") (groups m) ++ go (matchEnd m) after ms
-- ANCHOR_END: keeping

-- ANCHOR: named
-- 📛 Every named group that found something, by name.
-- 何かを見つけた名前付きのグループの全部を、名前で引ける表に
namedGroups :: Match -> Map Text Text
namedGroups m = Map.mapMaybe (\n -> group n m) (matchNames m)

-- ⚙️ Lines like "name = Mira" into a table. Lines of another shape
--    are skipped.
-- "name = Mira" のような行を、表にする。ほかの形の行は飛ばす
settings :: Text -> Map Text Text
settings t = Map.fromList
  [ (k, v)
  | m <- findAll line t
  , let found = namedGroups m
  , Just k <- [Map.lookup "key" found]
  , Just v <- [Map.lookup "value" found] ]
  where
    line = either (error . show) id
      (compileWith [Multiline] "^(?<key>[a-z]+) *= *(?<value>[^\n]*)$")
-- ANCHOR_END: named
-- ANCHOR_END: all
