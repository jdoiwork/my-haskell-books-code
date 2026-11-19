-- ANCHOR: all
-- | The same matcher, written with continuations: each part is given
--   "the rest of the work" and calls it from the place where it ends.
--   Now the rest of the work is also given what the groups have found
--   so far, and a group writes down what it found before it calls the
--   rest.
-- 同じマッチャーを、継続で書いたもの。どの部分も「残りの仕事」を
-- 受け取り、自分が終わった場所から、それを呼ぶ。この章から、残りの仕事には、
-- グループがここまでに見つけたものも渡す。グループは、見つけたものを
-- 書き留めてから、残りの仕事を呼ぶ
module MiniRegex.Continuation
  ( Rest
  , matchAt
  , run
  ) where

-- ANCHOR: imports
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.List.NonEmpty qualified as NE

import MiniRegex.Caps
import MiniRegex.Cursor
import MiniRegex.Groups
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (Option, Span (..))
-- ANCHOR_END: imports

-- ANCHOR: rest
-- 📮 The rest of the work: from here, with what the groups have found
--    so far, finish (and say where the whole match ends, and what the
--    groups found in the end), or fail.
-- 残りの仕事：ここから、グループがここまでに見つけたものを持って、
-- 先を終えて、一致全体の終わりと、グループが最後に見つけたものを言うか、
-- 失敗するか
type Rest = Cursor -> Caps -> Maybe (Cursor, Caps)

-- 🎯 Where the pattern ends, starting at the cursor, and what the
--    groups found (the first try that works). At the very end, the
--    rest of the work is to say "here, with these".
-- しおりから始めて、パターンが終わる場所と、グループが見つけたもの
-- （最初にうまくいった試し）。いちばん最後の残りの仕事は、「ここで、
-- これを持って」と言うこと
matchAt :: [Option] -> Pattern -> Cursor -> Maybe (Cursor, Caps)
matchAt opts p c = run opts p (\c' caps -> Just (c', caps)) c noCaps
-- ANCHOR_END: rest

-- ANCHOR: run
-- 🏃 Run the pattern, then the rest of the work. The groups are
--    numbered as they go: `n` is the number of the next group to open.
-- パターンを動かしてから、残りの仕事をする。グループの番号は、
-- 進みながら付ける。n は、次に開くグループの番号
run :: [Option] -> Pattern -> Rest -> Rest
run opts = pattern 1
  where
    -- The left branch first; the next one only when it fails. Each
    -- branch's first group has the number after the groups of the
    -- branches before it.
    -- 左の枝が先。失敗したときだけ、次の枝。どの枝の最初のグループも、
    -- 前の枝のグループのあとの番号
    pattern n (Pattern branches) k c caps =
      asum [ branch m b k c caps
           | (m, b) <- numbered n branchGroups (NE.toList branches) ]

    -- The pieces one after another: each one's rest of the work is
    -- the pieces after it, and then k.
    -- 部品を順に。どの部品の残りの仕事も、うしろの部品と、そのあとの k
    branch n (Branch ps) k =
      foldr (\(m, p) k' -> piece m p k') k (numbered n pieceGroups ps)

    piece _ (Anchor a) k c caps
      | holds opts a c = k c caps
      | otherwise      = Nothing
    piece n (Quantified a q) k c caps = repeatAtom q (atom n a) k c caps
-- ANCHOR_END: run

    -- ANCHOR: atom
    -- A group runs its pattern; one that keeps what it found writes it
    -- down, from where it started to where the pattern ended, before
    -- the rest of the work.
    -- グループは、中のパターンを動かす。見つけたものを覚えるグループは、
    -- 始めた場所から、パターンが終わった場所までを書き留めてから、
    -- 残りの仕事をする
    atom n (Group NonCapturing inner) k c caps = pattern n inner k c caps
    atom n (Group _ inner) k c caps =
      pattern (n + 1) inner (\c' caps' -> k c' (remember n c c' caps')) c caps
    -- \1: the same text again. A group that has found nothing makes it
    -- fail (PCRE2's rule).
    -- \1：同じ文字列を、もう一度。まだ何も見つけていないグループなら、
    -- 失敗（PCRE2 の決まり）
    atom _ (Backref r) k c caps = do
      s <- recall r caps
      c' <- stepOver (spanText s) c
      k c' caps
    atom _ a k c caps = case step c of
      Just (x, c') | fitsChar opts a x -> k c' caps
      _                                -> Nothing
    -- ANCHOR_END: atom

-- ANCHOR: numbered
-- 🔢 Give each part the number of its first group: the number after
--    all the groups in the parts before it.
-- 部分ごとに、最初のグループの番号を付ける：前の部分の、全部のグループの
-- 次の番号
numbered :: Int -> (a -> [Capture]) -> [a] -> [(Int, a)]
numbered n groupsOf xs = zip (scanl (+) n (map (length . groupsOf) xs)) xs
-- ANCHOR_END: numbered

-- ANCHOR: repeat
-- 🔁 The same order as the list matcher: greedy tries one more time
--    first, lazy tries the rest of the work first.
-- 候補のリストのマッチャーと同じ順番。欲ばりは「もう1回」を先に、
-- 控えめは「残りの仕事」を先に試す
repeatAtom
  :: Quantifier -> (Rest -> Rest) -> Rest -> Rest
repeatAtom (Quantifier (Count least more) greed) one k = go least more
  where
    go must left c caps
      | lastOfMany     = again
      | must > 0       = one (go (must - 1) left) c caps
      | left == Just 0 = k c caps
      | otherwise      = case greed of
          Greedy     -> again <|> k c caps
          Lazy       -> k c caps <|> again
          Possessive -> notYet
      where
        -- the last time that must be, of a+ or a{2,}: it is like one
        -- more time
        -- a+ や a{2,} の、最後の「必ず」の1回は、「もう1回」と同じ
        lastOfMany = must == 1 && left == Nothing
        again = one next c caps
        -- With no upper end (*, +, {2,}), a time that took nothing
        -- does not go round again (it would, for ever). With one
        -- ({2,5}), the empty times count too, up to the end (PCRE2
        -- does the same).
        -- 上の端がなければ（*・+・{2,}）、何も食べなかった1回のあとは、
        -- もう回らない（永遠に回ってしまう）。上の端があれば（{2,5}）、
        -- 空の回も数えて、端まで回る（PCRE2 と同じ）
        next c' caps'
          | left == Nothing && at c' == at c = k c' caps'
          | otherwise = go 0 (fmap (subtract 1) left) c' caps'

-- 🚧 compilePattern lets no possessive quantifier through yet.
-- 所有的な量指定子は、まだ compilePattern が通さない
notYet :: a
notYet = error "the matcher got a possessive quantifier"
-- ANCHOR_END: repeat
-- ANCHOR_END: all
