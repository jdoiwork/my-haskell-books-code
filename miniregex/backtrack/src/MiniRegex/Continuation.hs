-- ANCHOR: all
-- ANCHOR: header
-- | The same matcher, written with continuations: each part is given
--   "the rest of the work" and calls it from the place where it ends.
--   The first place where the rest says yes is the answer; no list of
--   candidates is made.
-- 同じマッチャーを、継続で書いたもの。どの部分も「残りの仕事」を
-- 受け取り、自分が終わった場所から、それを呼ぶ。残りの仕事が「はい」と
-- 言った最初の場所が答え。候補のリストは作らない
module MiniRegex.Continuation
  ( Rest
  , matchAt
  , run
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.List.NonEmpty qualified as NE

import MiniRegex.Cursor
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (Option)
-- ANCHOR_END: imports

-- ANCHOR: rest
-- 📮 The rest of the work: from here, finish (and say where the whole
--    match ends), or fail.
-- 残りの仕事：ここから先を終えて、一致全体の終わりを言うか、失敗するか
type Rest = Cursor -> Maybe Cursor

-- 🎯 Where the pattern ends, starting at the cursor (the first try that
--    works). The rest of the work, at the very end, is to say "here".
-- しおりから始めて、パターンが終わる場所（最初にうまくいった試し）。
-- いちばん最後の残りの仕事は、「ここ」と言うこと
matchAt :: [Option] -> Pattern -> Cursor -> Maybe Cursor
matchAt opts p = run opts p Just
-- ANCHOR_END: rest

-- ANCHOR: run
-- 🏃 Run the pattern, then the rest of the work.
-- パターンを動かしてから、残りの仕事をする
run :: [Option] -> Pattern -> Rest -> Rest
run opts = pattern
  where
    -- The left branch first; the next one only when it fails.
    -- 左の枝が先。失敗したときだけ、次の枝
    pattern (Pattern branches) k c =
      asum [branch b k c | b <- NE.toList branches]

    -- The pieces one after another: each one's rest of the work is
    -- the pieces after it, and then k.
    -- 部品を順に。どの部品の残りの仕事も、うしろの部品と、そのあとの k
    branch (Branch ps) k = foldr piece k ps

    piece (Anchor a) k c
      | holds opts a c = k c
      | otherwise      = Nothing
    piece (Quantified a q) k c = repeatAtom q (atom a) k c

    atom (Group _ inner) k c = pattern inner k c
    atom a k c = case step c of
      Just (x, c') | fitsChar opts a x -> k c'
      _                                -> Nothing
-- ANCHOR_END: run

-- ANCHOR: repeat
-- 🔁 The same order as the list matcher: greedy tries one more time
--    first, lazy tries the rest of the work first.
-- 候補のリストのマッチャーと同じ順番。欲ばりは「もう1回」を先に、
-- 控えめは「残りの仕事」を先に試す
repeatAtom
  :: Quantifier -> (Rest -> Rest) -> Rest -> Rest
repeatAtom (Quantifier (Count least more) greed) one k = go least more
  where
    go must left c
      | must > 0       = one (go (must - 1) left) c
      | left == Just 0 = k c
      | otherwise      = case greed of
          Greedy     -> again <|> k c
          Lazy       -> k c <|> again
          Possessive -> notYet
      where
        again = one next c
        next c'
          | at c' == at c = k c'
          | otherwise     = go 0 (fmap (subtract 1) left) c'

-- 🚧 compilePattern lets no possessive quantifier through yet.
-- 所有的な量指定子は、まだ compilePattern が通さない
notYet :: a
notYet = error "the matcher got a possessive quantifier"
-- ANCHOR_END: repeat
-- ANCHOR_END: all
