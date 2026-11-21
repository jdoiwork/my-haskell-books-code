-- ANCHOR: all
-- | The first matcher: from a cursor, list every place where the
--   pattern can end, in the order PCRE2 tries them, each with what the
--   groups found on the way there. The first one is the answer;
--   laziness keeps the others from being made, until they are needed.
--   An atomic group and a look use only the first of their inside.
-- 最初のマッチャー：しおりから、パターンが終われる場所を全部、PCRE2 が
-- 試す順に並べる。どの候補にも、そこまでの道でグループが見つけたものを
-- 添える。先頭が答え。遅延評価のおかげで、残りの候補は、要るまで
-- 作られない。アトミックグループと「見る」ものは、中の候補の先頭だけを使う
module MiniRegex.Candidates
  ( ends
  ) where

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)

import MiniRegex.Caps
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Grapheme (clusterEnd)
import MiniRegex.Groups
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (Span (..))
import MiniRegex.Unicode (sameCase)
import MiniRegex.Width (Width (..), branchWidth)
-- ANCHOR_END: imports

-- ANCHOR: ends
-- 🧺 Every place where the pattern can end, starting at the cursor,
--    and what the groups found on the way.
-- しおりから始めて、パターンが終われる場所の全部と、その道でグループが
-- 見つけたもの
ends :: Pattern -> Cursor -> [(Cursor, Caps)]
ends p c = pattern 1 p (c, noCaps)
  where
    -- The left branch first, then the next one.
    -- 左の枝が先、それから次の枝
    pattern n (Pattern branches) here =
      concat [ branch m b here
             | (m, b) <- numbered n branchGroups (NE.toList branches) ]

    -- (?| ... ): every branch starts at the same number
    -- (?| ... )：どの枝も、同じ番号から
    reset n (Pattern branches) here =
      concat [ branch n b here | b <- NE.toList branches ]

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
      | holds a cur = [here]
      | otherwise        = []
    piece _ KeepOut (cur, caps) = [(cur, keepOut cur caps)]
    -- a*+: only the first candidate of a* (the longest)
    -- a*+：a* の候補の先頭（いちばん長いもの）だけ
    piece n (Quantified a q) here
      | greed q == Possessive = take 1 (repeatAtom q (atom n a) here)
      | otherwise             = repeatAtom q (atom n a) here

    -- One character, a whole pattern for a group (writing down what it
    -- found), or the text a group found before.
    -- 1文字。グループなら中のパターン全体（見つけたものを書き留める）。
    -- 後方参照なら、グループが前に見つけた文字列
    atom n (Group NonCapturing inner) here = pattern n inner here
    atom n (Group BranchReset inner) here = reset n inner here
    atom n (Group _ inner) here@(from, _) =
      [ (to, remember n from to caps)
      | (to, caps) <- pattern (n + 1) inner here ]
    atom _ (Backref r) (cur, caps) = case recall r caps of
      Just s | Just cur' <- stepOver (spanText s) cur -> [(cur', caps)]
      _                                               -> []
    atom _ (NoCase (Backref r)) (cur, caps) = case recall r caps of
      Just s | Just cur' <- stepOverBy sameCase (spanText s) cur ->
        [(cur', caps)]
      _ -> []
    -- ANCHOR: look
    -- (?>...): the first candidate only. A look: is there a first
    -- candidate? Then stay where we are.
    -- (?>...)：候補の先頭だけ。見るもの：先頭の候補があるか。そのあと、
    -- いまの場所にとどまる
    atom n (Atomic inner) here = take 1 (pattern n inner here)
    atom n (Look Ahead sense inner) here =
      verdict sense (pattern n inner here) here
    atom n (Look Behind sense inner) here =
      verdict sense (lookBehind n inner here) here
    -- a call: not in this matcher (the chapter on recursion)
    -- 呼び出し：このマッチャーにはない（再帰の章）
    atom _ (Call _) _ =
      error "the list of candidates cannot call a group"
    -- \X: the one place where the cluster ends
    -- \X：クラスターの終わる場所1つ
    atom _ Cluster (cur, caps) = [(end, caps) | Just end <- [clusterEnd cur]]
    atom _ a (cur, caps) = case step cur of
      Just (x, cur') | fitsChar a x -> [(cur', caps)]
      _                                  -> []

    -- From the farthest place each branch can start; keep the ones that
    -- end just here.
    -- 枝ごとに、始まれるいちばん遠い場所から。ちょうどここで終わる候補だけ
    lookBehind n (Pattern branches) (cur, caps) =
      [ (end, caps')
      | (m, b) <- numbered n branchGroups (NE.toList branches)
      , d <- farthestFirst (branchWidth b)
      , Just start <- [back d cur]
      , (end, caps') <- branch m b (start, caps)
      , at end == at cur ]

-- ⚖️ A positive look goes on with the first candidate's groups; a
--    negative one, when there is no candidate.
-- 肯定は、先頭の候補のグループを持って先へ。否定は、候補がないときだけ
verdict :: Sense -> [(Cursor, Caps)] -> (Cursor, Caps) -> [(Cursor, Caps)]
verdict Positive ((_, caps') : _) (cur, _) = [(cur, caps')]
verdict Negative []               here     = [here]
verdict _        _                _        = []

-- 📏 How far back a branch of a lookbehind can start: the farthest first.
-- 後読みの枝が、どこまで下がって始まれるか。遠いほうから
farthestFirst :: Width -> [Int]
farthestFirst (Width least most) = [far, far - 1 .. near]
  where
    near = fromIntegral least
    far  = fromIntegral (fromMaybe least most)

-- ANCHOR_END: look

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
          Lazy -> here : again
          -- Greedy; Possessive too (piece takes the first one)
          -- 欲ばり。所有的も（piece が先頭だけを取る）
          _    -> again ++ [here]
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

-- ANCHOR_END: repeat
-- ANCHOR_END: all
