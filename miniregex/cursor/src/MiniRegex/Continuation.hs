-- ANCHOR: all
-- | The same matcher, written with continuations: each part is given
--   "the rest of the work" and calls it from the place where it ends.
--   The rest of the work is also given what the groups have found so
--   far. In this chapter, some parts run their inside with a rest of
--   the work of their own ("stop here, and say where"), and go on with
--   the answer: an atomic group, a lookahead and a lookbehind.
-- 同じマッチャーを、継続で書いたもの。どの部分も「残りの仕事」を
-- 受け取り、自分が終わった場所から、それを呼ぶ。残りの仕事には、
-- グループがここまでに見つけたものも渡す。この章では、中を自分だけの
-- 残りの仕事（「ここで止まって、場所を言う」）で動かし、その答えで先へ
-- 進む部分が加わる：アトミックグループ、先読み、後読み
module MiniRegex.Continuation
  ( Rest
  , matchAt
  , run
  , farthestFirst
  , numbered
  ) where

-- ANCHOR: imports
import Control.Applicative ((<|>))
import Control.Exception (throw)
import Data.Foldable (asum)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)

import MiniRegex.Caps
import MiniRegex.Cursor hiding (before, rest)
import MiniRegex.Groups
import MiniRegex.Rules
import MiniRegex.Syntax
import MiniRegex.Types (GaveUp (..), Span (..))
import MiniRegex.Unicode (sameCase)
import MiniRegex.Width (Width (..), branchWidth)
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
matchAt :: Pattern -> Cursor -> Maybe (Cursor, Caps)
matchAt p c = run p done c noCaps

-- 🏁 The last rest of the work: "here, with these".
-- いちばん最後の残りの仕事：「ここで、これを持って」
done :: Rest
done c caps = Just (c, caps)
-- ANCHOR_END: rest

-- ANCHOR: run
-- 🏃 Run the pattern, then the rest of the work. The groups are
--    numbered as they go: `n` is the number of the next group to open.
-- パターンを動かしてから、残りの仕事をする。グループの番号は、
-- 進みながら付ける。n は、次に開くグループの番号
run :: Pattern -> Rest -> Rest
run whole = matcher [] 1 whole
  where
    -- 🧭 The matcher, knowing the calls it is in: (group, place, the
    --    groups then), the latest first. A call runs the same matcher
    --    again, with one more call on the list (the chapter on
    --    recursion).
    -- 呼び出しの中にいることを知っているマッチャー：（グループ, 場所,
    -- そのときのグループの値）、新しいものが先頭。呼び出しは、リストに
    -- 1つ積んで、同じマッチャーをもう一度動かす（再帰の章）
    matcher :: [(Int, Int, Caps)] -> Int -> Pattern -> Rest -> Rest
    matcher inside = pattern
      where
-- ANCHOR_END: run
        -- The left branch first; the next one only when it fails. Each
        -- branch's first group has the number after the groups of the
        -- branches before it.
        -- 左の枝が先。失敗したときだけ、次の枝。どの枝の最初のグループも、
        -- 前の枝のグループのあとの番号
        pattern n (Pattern branches) k c caps =
          asum [ branch m b k c caps
               | (m, b) <- numbered n branchGroups (NE.toList branches) ]

        -- (?| ... ): every branch starts at the same number
        -- (?| ... )：どの枝も、同じ番号から
        reset n (Pattern branches) k c caps =
          asum [ branch n b k c caps | b <- NE.toList branches ]

        -- The pieces one after another: each one's rest of the work is
        -- the pieces after it, and then k.
        -- 部品を順に。どの部品の残りの仕事も、うしろの部品と、そのあとの k
        branch n (Branch ps) k =
          foldr (\(m, p) k' -> piece m p k') k (numbered n pieceGroups ps)

        piece _ (Anchor a) k c caps
          | holds a c = k c caps
          | otherwise      = Nothing
        -- \K: the match is to start here; write it down, and go on
        -- \K：一致は、ここから始まることにする。書き留めて、先へ
        piece _ KeepOut k c caps = k c (keepOut c caps)
        -- a*+ is (?>a*): repeat greedily, once, and never again
        -- a*+ は (?>a*)：欲ばってくり返すのを1回だけ。やり直さない
        piece n (Quantified a written) k c caps
          | greed q == Possessive =
              atomically (repeatAtom q (atom n a)) k c caps
          | otherwise = repeatAtom q (atom n a) k c caps
          where
            -- (?=…)+ goes round once more at most: lookCount in Rules
            -- (?=…)+ は、多くても、もう1回だけ回る（Rules の lookCount）
            q = lookCount a written

        -- ANCHOR: atom
        -- A group runs its pattern; one that keeps what it found writes it
        -- down, from where it started to where the pattern ended, before
        -- the rest of the work.
        -- グループは、中のパターンを動かす。見つけたものを覚えるグループは、
        -- 始めた場所から、パターンが終わった場所までを書き留めてから、
        -- 残りの仕事をする
        atom n (Group NonCapturing inner) k c caps = pattern n inner k c caps
        atom n (Group BranchReset inner) k c caps = reset n inner k c caps
        atom n (Group _ inner) k c caps =
          pattern (n + 1) inner
            (\c' caps' -> k c' (remember n c c' caps')) c caps
        -- \1: the same text again. A group that has found nothing makes it
        -- fail (PCRE2's rule).
        -- \1：同じ文字列を、もう一度。まだ何も見つけていないグループなら、
        -- 失敗（PCRE2 の決まり）
        atom _ (Backref r) k c caps = do
          s <- recall r caps
          c' <- stepOver (spanText s) c
          k c' caps
        -- (?i)\1: the same text, but A and a are the same
        -- (?i)\1：同じ文字列。ただし A と a は同じ
        atom _ (NoCase (Backref r)) k c caps = do
          s <- recall r caps
          c' <- stepOverBy sameCase (spanText s) c
          k c' caps
        -- ANCHOR: look
        -- (?>...): the first way the inside works, and only that one
        -- (?>...)：中がうまくいった最初のやり方だけ
        atom n (Atomic inner) k c caps = atomically (pattern n inner) k c caps
        -- (?=...) and (?!...): run the inside from here, on its own; then
        -- go on from here (not from where the inside ended). A positive
        -- look keeps what its groups found; a negative one has none.
        -- (?=...) と (?!...)：ここから中を、自分だけで動かす。そのあと、
        -- （中が終わった所ではなく）ここから先へ。肯定なら、中のグループが
        -- 見つけたものを持っていく。否定なら、見つけたものはない
        atom n (Look Ahead sense inner) k c caps =
          verdict sense (pattern n inner done c caps) k c caps
        -- (?<=...) and (?<!...): step back, run the inside, and ask that it
        -- ends just here
        -- (?<=...) と (?<!...)：後ろへ下がって中を動かし、ちょうどここで
        -- 終わることを求める
        atom n (Look Behind sense inner) k c caps =
          verdict sense (lookBehind n inner c caps) k c caps
        -- ANCHOR_END: look
        -- ANCHOR: call
        -- (?1): run group 1's pattern from here, inside one more call;
        -- coming back, the groups are as they were before the call.
        -- The same group at the same place, with the same groups, again
        -- would go round for ever: give up (PCRE2 gives up, too).
        -- (?1)：1番のグループのパターンを、ここから、呼び出しを1つ
        -- 積んで動かす。戻ってきたら、グループは呼ぶ前の値に。同じ
        -- グループを、同じ場所で、同じグループの値のまま、また呼ぶと、
        -- いつまでも回るので、あきらめる（PCRE2 も、あきらめる）
        atom _ (Call g) k c caps
          | looping   = throw (Looping g (at c))
          | otherwise = case groupBody g whole of
              Just inner ->
                matcher ((g, at c, caps) : inside) (g + 1) inner
                  (\c' caps' -> k c' (returning caps caps')) c caps
              Nothing -> Nothing  -- (not reached / 来ない)
          where
            -- the same group, place and groups as a call before
            -- 前の呼び出しと、同じグループ・場所・グループの値
            -- (only the calls at this same place, at the list's head)
            -- （見るのは、リストの頭の、同じ場所の呼び出しだけ）
            looping = (g, caps) `elem`
              [ (h, cs)
              | (h, _, cs) <- takeWhile (\(_, p, _) -> p == at c) inside ]
        -- ANCHOR_END: call
        atom _ a k c caps = case step c of
          Just (x, c') | fitsChar a x -> k c' caps
          _                                -> Nothing
        -- ANCHOR_END: atom

        -- ANCHOR: behind
        -- Each branch on its own: from the farthest place it can start,
        -- one step nearer at a time (PCRE2's order). The inside must end
        -- where we are.
        -- 枝ごとに、始まれるいちばん遠い場所から、1つずつ近づく（PCRE2 と
        -- 同じ順）。中は、いまいる場所で終わらなければならない
        lookBehind n (Pattern branches) c caps =
          asum [ do start <- back d c
                    branch m b endsHere start caps
               | (m, b) <- numbered n branchGroups (NE.toList branches)
               , d <- farthestFirst (branchWidth b) ]
          where
            endsHere c' caps'
              | at c' == at c = Just (c', caps')
              | otherwise     = Nothing
        -- ANCHOR_END: behind

-- ANCHOR: atomically
-- 🔒 Run a part with its own rest of the work, which only says where
--    it stopped; then go on with that answer. If the rest of the work
--    fails later, nothing goes back into the part: there is no other
--    answer to try.
-- 部分を、自分だけの残りの仕事（止まった場所を言うだけ）で動かし、
-- その答えで先へ進む。あとで残りの仕事が失敗しても、部分の中へは
-- 戻らない。試す答えが、ほかにないから
atomically :: (Rest -> Rest) -> Rest -> Rest
atomically part k c caps = do
  (c', caps') <- part done c caps
  k c' caps'

-- ⚖️ What a look says: a positive one goes on when the inside worked
--    (with its groups), a negative one when it did not (with none).
-- 見た結果：肯定は中がうまくいったら（中のグループを持って）、否定は
-- うまくいかなかったら（何も持たずに）先へ進む
verdict :: Sense -> Maybe (Cursor, Caps) -> Rest -> Rest
verdict Positive (Just (_, caps')) k c _    = k c caps'
verdict Negative Nothing           k c caps = k c caps
verdict _        _                 _ _ _    = Nothing
-- 📏 How far back a branch of a lookbehind can start: the farthest
--    first. (The parser lets no branch with no limit in.)
-- 後読みの枝が、どこまで下がって始まれるか。遠いほうから。
-- （上限のない枝は、パーサーが入れない）
farthestFirst :: Width -> [Int]
farthestFirst (Width least most) = [far, far - 1 .. near]
  where
    near = fromIntegral least
    far  = fromIntegral (fromMaybe least most)
-- ANCHOR_END: atomically

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
          Lazy -> k c caps <|> again
          -- Greedy; and Possessive, which `piece` runs atomically
          -- 欲ばり。所有的も（piece がアトミックに動かす）
          _    -> again <|> k c caps
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

-- ANCHOR_END: repeat
-- ANCHOR_END: all
