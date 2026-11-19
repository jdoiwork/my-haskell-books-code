-- ANCHOR: all
-- | The first matcher: from a cursor, list every place where the
--   pattern can end, in the order PCRE2 tries them, each with what the
--   groups found on the way there. The first one is the answer;
--   laziness keeps the others from being made, until they are needed.
-- 最初のマッチャー：しおりから、パターンが終われる場所を全部、PCRE2 が
-- 試す順に並べる。どの候補にも、そこまでの道でグループが見つけたものを
-- 添える。先頭が答え。遅延評価のおかげで、残りの候補は、要るまで
-- 作られない
module MiniRegex.Candidates
  ( ends
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE

import MiniRegex.Caps
import MiniRegex.Cursor
import MiniRegex.Groups
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (Option, Span (..))
-- ANCHOR_END: imports

-- ANCHOR: ends
-- 🧺 Every place where the pattern can end, starting at the cursor,
--    and what the groups found on the way.
-- しおりから始めて、パターンが終われる場所の全部と、その道でグループが
-- 見つけたもの
ends :: [Option] -> Pattern -> Cursor -> [(Cursor, Caps)]
ends opts p c = pattern 1 p (c, noCaps)
  where
    -- The left branch first, then the next one.
    -- 左の枝が先、それから次の枝
    pattern n (Pattern branches) here =
      concat [ branch m b here
             | (m, b) <- numbered n branchGroups (NE.toList branches) ]

    -- The pieces one after another: where the first ends, the next
    -- starts.
    -- 部品を順に。前の部品が終わった場所から、次の部品が始まる
    branch n (Branch ps) here = go (numbered n pieceGroups ps) here
      where
        go [] h = [h]
        go ((m, q) : qs) h = do
          h' <- piece m q h
          go qs h'

    piece _ (Anchor a) here@(cur, _)
      | holds opts a cur = [here]
      | otherwise        = []
    piece n (Quantified a q) here = repeatAtom q (atom n a) here

    -- One character, a whole pattern for a group (writing down what it
    -- found), or the text a group found before.
    -- 1文字。グループなら中のパターン全体（見つけたものを書き留める）。
    -- 後方参照なら、グループが前に見つけた文字列
    atom n (Group NonCapturing inner) here = pattern n inner here
    atom n (Group _ inner) here@(from, _) =
      [ (to, remember n from to caps)
      | (to, caps) <- pattern (n + 1) inner here ]
    atom _ (Backref r) (cur, caps) = case recall r caps of
      Just s | Just cur' <- stepOver (spanText s) cur -> [(cur', caps)]
      _                                               -> []
    atom _ a (cur, caps) = case step cur of
      Just (x, cur') | fitsChar opts a x -> [(cur', caps)]
      _                                  -> []

-- 🔢 Give each part the number of its first group.
-- 部分ごとに、最初のグループの番号を付ける
numbered :: Int -> (a -> [Capture]) -> [a] -> [(Int, a)]
numbered n groupsOf xs = zip (scanl (+) n (map (length . groupsOf) xs)) xs
-- ANCHOR_END: ends

-- ANCHOR: repeat
-- 🔁 Repeat `one`: first the times that must be, then the extra ones.
--    Greedy puts "one more time" before "stop here"; lazy puts it
--    after. One more time that took nothing does not go round again.
-- one をくり返す。まず、少なくともの回数。そのあと、あと何回か。
-- 欲ばりは「もう1回」を「ここでやめる」の前に、控えめは、うしろに
-- 並べる。何も食べなかった1回のあとは、もう回らない
repeatAtom
  :: Quantifier -> ((Cursor, Caps) -> [(Cursor, Caps)])
  -> (Cursor, Caps) -> [(Cursor, Caps)]
repeatAtom (Quantifier (Count least more) greed) one = go least more
  where
    go must left here@(c, _)
      | lastOfMany     = again
      | must > 0       = do
          here' <- one here
          go (must - 1) left here'
      | left == Just 0 = [here]
      | otherwise      = case greed of
          Greedy     -> again ++ [here]
          Lazy       -> here : again
          Possessive -> notYet
      where
        -- the last time that must be, of a+ or a{2,}
        -- a+ や a{2,} の、最後の「必ず」の1回
        lastOfMany = must == 1 && left == Nothing
        -- with no upper end, a time that took nothing does not go
        -- round again; with one, the empty times count too
        -- 上の端がなければ、何も食べなかった1回のあとは、もう回らない。
        -- 上の端があれば、空の回も数える
        again = do
          here'@(c', _) <- one here
          if left == Nothing && at c' == at c
            then [here']
            else go 0 (fmap (subtract 1) left) here'

-- 🚧 compilePattern lets no possessive quantifier through yet.
-- 所有的な量指定子は、まだ compilePattern が通さない
notYet :: a
notYet = error "the matcher got a possessive quantifier"
-- ANCHOR_END: repeat
-- ANCHOR_END: all
