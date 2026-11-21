-- ANCHOR: all
-- ANCHOR: header
-- | The first matcher: from a cursor, list every place where the
--   pattern can end, in the order PCRE2 tries them. The first one is
--   the answer; laziness keeps the others from being made, until they
--   are needed.
-- 最初のマッチャー：しおりから、パターンが終われる場所を全部、PCRE2 が
-- 試す順に並べる。先頭が答え。遅延評価のおかげで、残りの候補は、
-- 要るまで作られない
module MiniRegex.Candidates
  ( ends
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Data.List.NonEmpty qualified as NE

import MiniRegex.Cursor
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (Option)
-- ANCHOR_END: imports

-- ANCHOR: ends
-- ANCHOR: top
-- 🧺 Every place where the pattern can end, starting at the cursor.
-- しおりから始めて、パターンが終われる場所の全部
ends :: [Option] -> Pattern -> Cursor -> [Cursor]
ends opts = pattern
  where
-- ANCHOR_END: top
    -- ANCHOR: pattern
    -- The left branch first, then the next one.
    -- 左の枝が先、それから次の枝
    pattern (Pattern branches) c =
      concatMap (\b -> branch b c) (NE.toList branches)

    -- The pieces one after another: where the first ends, the next
    -- starts.
    -- 部品を順に。前の部品が終わった場所から、次の部品が始まる
    branch (Branch []) c = [c]
    branch (Branch (p : ps)) c = do
      c' <- piece p c
      branch (Branch ps) c'

    piece (Anchor a) c
      | holds opts a c = [c]
      | otherwise      = []
    piece (Quantified a q) c = repeatAtom q (atom a) c
    -- ANCHOR_END: pattern

    -- ANCHOR: atom
    -- One character, or a whole pattern for a group.
    -- 1文字。グループなら、中のパターン全体
    atom (Group _ inner) c = pattern inner c
    atom a c = case step c of
      Just (x, c') | fitsChar opts a x -> [c']
      _                                -> []
    -- ANCHOR_END: atom
-- ANCHOR_END: ends

-- ANCHOR: repeat
-- 🔁 Repeat `one`: first the times that must be, then the extra ones.
--    Greedy puts "one more time" before "stop here"; lazy puts it
--    after. Only the order is different.
-- one をくり返す。まず、少なくともの回数。そのあと、あと何回か。
-- 欲ばりは「もう1回」を「ここでやめる」の前に、控えめは、うしろに並べる。
-- ちがうのは、並べる順番だけ
repeatAtom :: Quantifier -> (Cursor -> [Cursor]) -> Cursor -> [Cursor]
repeatAtom (Quantifier (Count least more) greed) one = go least more
  where
    go must left c
      | must > 0     = do
          c' <- one c
          go (must - 1) left c'
      | left == Just 0 = [c]
      | otherwise    = case greed of
          Greedy     -> again ++ [c]
          Lazy       -> c : again
          Possessive -> notYet
      where
        -- ANCHOR: again
        -- One more time. If it took nothing, do not go round again
        -- (it would take nothing for ever); stop just after it.
        -- もう1回。何も食べずに終わったら、もう回らない（永遠に何も
        -- 食べずに回り続けてしまう）。そのすぐうしろで止まる
        again = do
          c' <- one c
          if at c' == at c
            then [c']
            else go 0 (fmap (subtract 1) left) c'
        -- ANCHOR_END: again

-- 🚧 compilePattern lets no possessive quantifier through yet.
-- 所有的な量指定子は、まだ compilePattern が通さない
notYet :: a
notYet = error "the matcher got a possessive quantifier"
-- ANCHOR_END: repeat
-- ANCHOR_END: all
