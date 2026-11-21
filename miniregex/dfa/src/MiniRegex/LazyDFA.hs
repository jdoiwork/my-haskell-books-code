-- ANCHOR: all
-- ANCHOR: header
-- | A lazy DFA, as Rust's regex and RE2 have: the states of the machine
--   are made only when a character first needs them, and kept in a
--   cache. A forward DFA finds where a match ends; a reverse DFA, from
--   there, where it starts; the groups, if the pattern has any, come
--   from walking a program with one way only, or from the engine the
--   regex chose, on that span only, with its limit on the steps.
-- Rust の regex や RE2 と同じ、lazy DFA（その場で作る DFA）。機械の状態
-- は、ある文字で初めて要るときに作り、キャッシュに覚える。前向きの DFA で
-- 一致の終わりを、そこから逆向きの DFA で始まりを見つける。グループが
-- あれば、道が1つのプログラムなら1度たどって、そうでなければ、その範囲
-- だけで、正規表現が選んだエンジンに、ステップ数の上限つきで求めてもらう
module MiniRegex.LazyDFA
  ( Dfa (..)
  , dfaFor
  , canDfa
  , reversePattern
  , closure
  , dfaFindAll
  , dfaFind
  , stateLimit
  , isStraight
  , dfaCountEnds
  , dfaSpans
  , dfaStates
  ) where
-- ANCHOR_END: header

-- ANCHOR: imports
-- The ST monad, arrays and maps
-- ST モナド、配列、辞書
import Control.Monad.ST (ST, runST)
import Data.Array (bounds, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.STRef
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed.Mutable qualified as MU

-- Text, down to its bytes
-- Text。バイトまで降りて読む
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal (Text (..))
import Data.Text.Unsafe
  (Iter (..), dropWord8, iterArray, reverseIter, takeWord8)

-- MiniRegex
import MiniRegex.Caps (Caps, groupList, noCaps, remember)
import MiniRegex.Compile (compileTree)
import MiniRegex.Cursor (Cursor, byte, cursorAt, headOf, jumpTo, step)
import MiniRegex.Engine (searchAt)
import MiniRegex.Groups (groupCount, groupNames)
import MiniRegex.Program
import MiniRegex.Rules (eatAll, fitsChar)
import MiniRegex.Syntax (Atom (..), Branch (..), Pattern (..), Piece (..))
import MiniRegex.Types (Regex (..))
import MiniRegex.Types qualified as Ty
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🤖 What the DFA needs: the program, and the program of the reversed
--    pattern (to step back from the end to the start).
-- DFA に要るもの：プログラムと、逆にしたパターンのプログラム（終わりから
-- 始まりへ戻るため）
data Dfa = Dfa
  { forward  :: Program
  , backward :: Program
  }

-- 🧮 At most this many states in a cache: past it, the DFA gives up,
--    and the VM does the search (Rust's lazy DFA gives up too, when its
--    cache is no help).
-- キャッシュの状態は、多くてもこれだけ。超えたら DFA はあきらめ、VM が
-- 探す（Rust の lazy DFA も、キャッシュが役に立たないと、あきらめる）
stateLimit :: Int
stateLimit = 2000
-- ANCHOR_END: types

-- ANCHOR: can
-- ✅ The DFA runs only programs of take, takeall, split, jmp, open,
--    close and match, that never match the empty text. (No \b, ^, $:
--    they look at the character before; no \1, no looks, no fences.)
--    A takeall is the greedy repeat: the compiler makes one only where
--    that is the same (from the chapter on switching engines).
-- DFA が動かすのは、take・takeall・split・jmp・open・close・match だけで、
-- 空の文字列に一致しないプログラム。（\b・^・$ は前の文字を見るので
-- 断る。\1・「見る」もの・区切りも断る。）takeall は欲ばりのくり返し：
-- コンパイラーは、それと同じになるときにだけ takeall を作る（エンジンを
-- 乗り換える章から）
canDfa :: Program -> Bool
canDfa p = all ok [lo .. hi] && not (snd (closure True p [0]))
  where
    (lo, hi) = bounds p
    ok pc = case p ! pc of
      Take _    -> True
      TakeAll _ -> True
      Split _ _ -> True
      Jmp _     -> True
      Open _    -> True
      Close _   -> True
      Match     -> True
      _         -> False

-- 🤖 The DFA for a regex, if it can run it (and its reversed pattern).
-- 正規表現の DFA。動かせるなら（逆にしたパターンも）
dfaFor :: Regex -> Maybe Dfa
dfaFor re
  | canDfa (program re) && canDfa back = Just (Dfa (program re) back)
  | otherwise                         = Nothing
  where
    back = compileTree (reversePattern (tree re))
-- ANCHOR_END: can

-- ANCHOR: reverse
-- 🔁 The pattern backwards: every branch with its pieces in the other
--    order, and so inside the groups too. ab(c|de) becomes (c|ed)ba.
-- 逆向きのパターン：どの枝も部品を逆の順に。グループの中も同じ。
-- ab(c|de) は (c|ed)ba になる
reversePattern :: Pattern -> Pattern
reversePattern (Pattern bs) = Pattern (fmap branch bs)
  where
    branch (Branch ps) = Branch (reverse (map piece ps))
    piece (Quantified a q) = Quantified (atom a) q
    piece other            = other
    atom (Group c inner) = Group c (reversePattern inner)
    atom (NoCase a)      = NoCase (atom a)
    atom a               = a
-- ANCHOR_END: reverse

-- ANCHOR: closure
-- 🧵 From these instructions, follow the ones that take no character:
--    the takes (and takealls) we get to, in order, and whether a match
--    was reached. With `cut`, nothing after the match is kept: a thread
--    after it has a lower priority (leftmost-first, as RE2 does it).
-- この命令から、文字を食べない命令をたどる。着いた take（と takeall）を
-- 順に並べたものと、match に着いたか。cut なら、match のあとは残さない。
-- match よりあとのスレッドは優先が低いから（RE2 と同じ、左が先の決まり）
closure :: Bool -> Program -> [Int] -> ([Int], Bool)
closure cut p = go IntSet.empty [] False
  where
    go _ acc m [] = (reverse acc, m)
    go seen acc m (pc : more)
      | IntSet.member pc seen = go seen acc m more
      | otherwise = case p ! pc of
          Take _    -> go seen' (pc : acc) m more
          -- greedy: stay first, then go on
          -- 欲ばり：まずとどまり、それから先へ
          TakeAll _ -> go seen' (pc : acc) m (pc + 1 : more)
          Split a b -> go seen' acc m (a : b : more)
          Jmp a     -> go seen' acc m (a : more)
          Match
            | cut       -> (reverse acc, True)
            | otherwise -> go seen' acc True more
          _         -> go seen' acc m (pc + 1 : more)
      where
        seen' = IntSet.insert pc seen
-- ANCHOR_END: closure

-- ANCHOR: machine
-- 🔑 A state: its threads, whether new threads still start (no match
--    seen yet), and whether it matches. (Two states with the same
--    threads may differ in the last: a thread may have reached the match
--    on the way.)
-- 状態：スレッド、新しいスレッドをまだ始めるか（まだ一致を見ていない）、
-- 一致しているか。（同じスレッドでも、最後のものはちがうことがある。
-- 途中で match に着いたスレッドがあるかもしれない）
type Key = ([Int], Bool, Bool)

-- 🗄️ A DFA in the making: the states made so far, by their keys; for
--    each, whether it matches and whether it is dead (no thread, and no
--    new one to start); and the moves found so far: a table of 128 for
--    ASCII, and an IntMap for the rest.
-- 作りかけの DFA：これまでに作った状態（鍵で引く）、状態ごとに、一致して
-- いるか、死んでいるか（スレッドがなく、新しく始めもしない）、これまでに
-- 見つけた移り先（ASCII は 128 の表、ほかは IntMap）
data Machine s = Machine
  { code    :: Program
  , cutting :: Bool                        -- leftmost-first, or longest
  , ids     :: STRef s (Map.Map Key Int)
  , states  :: STRef s (MV.MVector s Key)
  , flags   :: STRef s (MU.MVector s Int)  -- 1: match, 2: dead
  , ascii   :: STRef s (MU.MVector s Int)  -- -1: not yet
  , others  :: STRef s (IntMap.IntMap Int)
  , count   :: STRef s Int
  , first   :: STRef s (Maybe Int)         -- the first state, once made
  }

newMachine :: Program -> Bool -> ST s (Machine s)
newMachine p cut =
  Machine p cut <$> newSTRef Map.empty <*> (newSTRef =<< MV.new 64)
                <*> (newSTRef =<< MU.replicate 64 0)
                <*> (newSTRef =<< MU.replicate (64 * 128) (-1))
                <*> newSTRef IntMap.empty <*> newSTRef 0
                <*> newSTRef Nothing

-- 🔢 The number of a state, made if it is new. Nothing when there are
--    too many.
-- 状態の番号。新しければ作る。多すぎたら Nothing
stateOf :: Machine s -> Key -> ST s (Maybe Int)
stateOf m key@(ths, starting, matched) = do
  known <- readSTRef (ids m)
  case Map.lookup key known of
    Just i  -> pure (Just i)
    Nothing -> do
      i <- readSTRef (count m)
      if i >= stateLimit then pure Nothing else do
        writeSTRef (count m) (i + 1)
        modifySTRef' (ids m) (Map.insert key i)
        sv <- roomFor (states m) MV.length MV.grow i
        MV.write sv i key
        fv <- roomFor (flags m) MU.length MU.grow i
        MU.write fv i ((if matched then 1 else 0)
                       + (if null ths && not starting then 2 else 0))
        av <- readSTRef (ascii m)
        if (i + 1) * 128 <= MU.length av then pure () else do
          bigger <- MU.grow av (MU.length av)
          MU.set (MU.slice (MU.length av) (MU.length av) bigger) (-1)
          writeSTRef (ascii m) bigger
        pure (Just i)
  where
    -- make the vector twice as big, if i does not fit
    -- i が入らなければ、ベクトルを倍にする
    roomFor ref size grow i = do
      v <- readSTRef ref
      if i < size v then pure v else do
        w <- grow v (size v)
        writeSTRef ref w
        pure w
-- ANCHOR_END: machine

-- ANCHOR: move
-- 👣 Where state i goes with the character c: from the table, or made
--    now (the threads that take c go on; while no match has been seen,
--    a new thread starts at 0, behind them all).
-- 状態 i が文字 c で移る先：表から、なければいま作る（c を食べる
-- スレッドが進む。まだ一致を見ていなければ、いちばん後ろに、0 から
-- 新しいスレッドを始める）
move :: Machine s -> Int -> Char -> ST s (Maybe Int)
move m i c
  | c < '\x80' = do
      a <- readSTRef (ascii m)
      j <- MU.read a (i * 128 + fromEnum c)
      if j >= 0 then pure (Just j) else do
        r <- make
        a' <- readSTRef (ascii m)
        mapM_ (MU.write a' (i * 128 + fromEnum c)) r
        pure r
  | otherwise = do
      o <- readSTRef (others m)
      let key = i * 0x110000 + fromEnum c
      case IntMap.lookup key o of
        Just j  -> pure (Just j)
        Nothing -> do
          r <- make
          mapM_ (modifySTRef' (others m) . IntMap.insert key) r
          pure r
  where
    p = code m
    make = do
      (ths, starting, _) <- (`MV.read` i) =<< readSTRef (states m)
      let next = concat [after pc | pc <- ths, fitsChar (atomOf pc) c]
          after pc = case p ! pc of
            TakeAll _ -> [pc, pc + 1]
            _         -> [pc + 1]
          (ths1, m1) = closure (cutting m) p next
          (ths2, m2)
            | starting && not m1 = closure (cutting m) p (next ++ [0])
            | otherwise          = (ths1, m1)
      stateOf m (ths2, starting && not m2, m2)
    atomOf pc = case p ! pc of
      Take a    -> a
      TakeAll a -> a
      _         -> error "not a take"

-- 🚪 The first state: the threads from 0. Made once, and kept: every
--    search starts from it.
-- 最初の状態：0 からのスレッド。1度だけ作って覚えておく。どの探索も
-- ここから始まる
firstState :: Machine s -> Bool -> ST s (Maybe Int)
firstState m starting = readSTRef (first m) >>= \case
  Just i  -> pure (Just i)
  Nothing -> do
    r <- stateOf m (ths, starting, matched)
    writeSTRef (first m) r
    pure r
  where
    (ths, matched) = closure (cutting m) (code m) [0]
-- ANCHOR_END: move

-- ANCHOR: forward
-- ➡️ Forward, from byte b0: where the leftmost-first match ends (in
--    bytes), Nothing if there is none, Left if the cache grew too big.
--    The flags and the ASCII table are read once, and again only after
--    a new state was made; the best end is kept strict.
-- 前向きに、b0 バイト目から：左が先の一致が終わる場所（バイト）。なければ
-- Nothing、キャッシュが大きくなりすぎたら Left。旗と ASCII の表は、最初と、
-- 新しい状態を作ったあとにだけ読む。いちばんよい終わりは正格に持つ
forwardEnd :: Machine s -> Text -> Int -> ST s (Either () (Maybe Int))
forwardEnd m (Text arr off len) b0 = firstState m True >>= \case
  Nothing -> pure (Left ())
  Just s0 -> fresh s0 b0 (-1)
  where
    fresh s b best = do
      f <- readSTRef (flags m)
      a <- readSTRef (ascii m)
      loop f a s b best
    -- f and a strict too: then GHC hands the loop what is inside the
    -- vectors, once, instead of opening the boxes at every character
    -- f と a も正格に。そうすると GHC は、ベクトルの中身を1度だけ
    -- ループに渡す。文字ごとに箱を開けなくて済む
    loop !f !a !s !b !best = do
      fl <- MU.unsafeRead f s
      let !best' = if odd fl then b else best
      if fl >= 2 || b >= len
        then pure (Right (if best' < 0 then Nothing else Just best'))
        else do
          let Iter c d = iterArray arr (off + b)
          j <- if c < '\x80' then MU.unsafeRead a (s * 128 + fromEnum c)
                             else pure (-1)
          if j >= 0 then loop f a j (b + d) best' else
            move m s c >>= \case
              Nothing -> pure (Left ())
              Just j' -> fresh j' (b + d) best'
-- ANCHOR_END: forward

-- ANCHOR: backward
-- ⬅️ Backward, from the end e to no earlier than byte lo, with the DFA of
--    the reversed pattern: the earliest place where it matches. That is
--    where the leftmost-first match starts (RE2 does the same).
-- 逆向きに、終わり e から、lo バイト目より前には行かずに、逆にした
-- パターンの DFA で：一致する、いちばん前の場所。それが、左が先の一致の
-- 始まり（RE2 も同じ）
backwardStart :: Machine s -> Text -> Int -> Int -> ST s (Either () Int)
backwardStart m t lo e = firstState m False >>= \case
  Nothing -> pure (Left ())
  Just s0 -> fresh s0 e e
  where
    fresh s b best = do
      f <- readSTRef (flags m)
      a <- readSTRef (ascii m)
      go f a s b best
    go !f !a !s !b !best = do
      fl <- MU.unsafeRead f s
      let !best' = if odd fl then b else best
      if fl >= 2 || b <= lo then pure (Right best') else do
        let Iter c d = reverseIter t (b - 1)
        j <- if c < '\x80' then MU.unsafeRead a (s * 128 + fromEnum c)
                           else pure (-1)
        if j >= 0 then go f a j (b + d) best' else
          move m s c >>= \case
            Nothing -> pure (Left ())
            Just j' -> fresh j' (b + d) best'
-- ANCHOR_END: backward

-- ANCHOR: find
-- 🔍 All the matches, one after another, with the DFA; Nothing if the
--    DFA cannot run the regex or gives up (then the VM does it all).
-- DFA で、一致を順に全部。DFA が動かせないか、あきらめたら Nothing
-- （そのときは VM が全部やる）
dfaFindAll :: Regex -> Text -> Maybe [Ty.Match]
dfaFindAll re t = do
  d <- dfaFor re
  either (const Nothing) Just (runST (matchesFrom re d t 0 0 maxBound))

-- 🔍 The first match from character `from`, the same way.
-- from 文字目からの最初の一致。同じやり方で
dfaFind :: Regex -> Text -> Int -> Maybe (Maybe Ty.Match)
dfaFind re t from = do
  d <- dfaFor re
  c <- cursorAt t from
  either (const Nothing) (Just . firstOne)
    (runST (matchesFrom re d t (byte c) from 1))
  where
    firstOne ms = case ms of
      m : _ -> Just m
      []    -> Nothing

-- 🔁 Up to n matches from byte b0 (character k0).
-- b0 バイト目（k0 文字目）から、多くて n 個の一致
matchesFrom
  :: Regex -> Dfa -> Text -> Int -> Int -> Int
  -> ST s (Either () [Ty.Match])
matchesFrom re (Dfa fw bw) t b0 k0 n0 = do
  fm <- newMachine fw True
  bm <- newMachine bw False
  let go b k n acc
        | n <= 0 = pure (Right (reverse acc))
        | otherwise = forwardEnd fm t b >>= \case
            Left ()        -> pure (Left ())
            Right Nothing  -> pure (Right (reverse acc))
            Right (Just e) -> backwardStart bm t b e >>= \case
              Left ()  -> pure (Left ())
              Right s  ->
                -- the characters, counted only now, over the bytes
                -- between (Text counts them quickly)
                -- 文字の数は、ここで、あいだのバイトを数える（Text は
                -- 速く数える）
                let !ks = k + chars b s
                    !ke = ks + chars s e
                in  go e ke (n - 1) (matchOf s ks e ke : acc)
  go b0 k0 n0 []
  where
    chars from to = T.length (takeWord8 (to - from) (dropWord8 from t))
    whole = case cursorAt t 0 of
      Just c  -> c
      Nothing -> error "no cursor"
    cursor b k = jumpTo k b whole
    -- worked out once for the regex, not again for each match
    -- 一致ごとではなく、正規表現ごとに1度だけ求める
    groups   = groupCount (tree re)
    names    = Map.fromList (groupNames (tree re))
    straight = isStraight (program re)
    matchOf s ks e ke = Ty.Match
      { Ty.matchText   = takeWord8 (e - s) (dropWord8 s t)
      , Ty.matchStart  = ks
      , Ty.matchEnd    = ke
      , Ty.matchGroups = groupsOf s ks e
      , Ty.matchNames  = names
      }
    groupsOf s ks e
      | groups == 0 = []
      -- one way only: walk it once
      -- 道が1つだけ：1度たどる
      | straight    =
          groupList groups (walkStraight (program re) (cursor s ks))
      -- or else the engine the regex chose, with its limit on the steps
      -- (GaveUp is thrown when they run out), from the start, on the
      -- text up to the end only: the same match, with groups. The table
      -- and the budget then cost only as much as the match.
      -- そうでなければ、正規表現が選んだエンジンで、ステップ数の上限つき
      -- （尽きたら GaveUp を投げる）。始まりから、文字列は終わりまでだけ：
      -- 同じ一致を、グループつきで。表も予算も、一致の長さの分で済む
      | otherwise = case searchAt re (jumpTo ks s (upTo e)) of
          Just (m, _) -> Ty.matchGroups m
          Nothing     -> error "the VM did not find the DFA's match"
    upTo e = headOf (takeWord8 e t)
-- ANCHOR_END: find

-- ANCHOR: straight
-- 🛤️ A program with no split and no jmp: one way only, so the groups
--    come from walking it once (the smallest one-pass DFA).
-- split も jmp もないプログラム：道は1つだけなので、1度たどれば
-- グループがわかる（いちばん小さい one-pass DFA）
isStraight :: Program -> Bool
isStraight p = all ok [lo .. hi]
  where
    (lo, hi) = bounds p
    ok pc = case p ! pc of
      Take _    -> True
      TakeAll _ -> True
      Open _    -> True
      Close _   -> True
      Match     -> True
      _         -> False

-- 🛤️ Walk it from the start of a match the DFA found: it cannot fail.
-- DFA が見つけた一致の始まりから、たどる。失敗することはない
walkStraight :: Program -> Cursor -> Caps
walkStraight p = go 0 IntMap.empty noCaps
  where
    go pc opens caps c = case p ! pc of
      Take _    -> maybe caps (go (pc + 1) opens caps . snd) (step c)
      TakeAll a -> go (pc + 1) opens caps (fst (eatAll a maxBound c))
      Open g    -> go (pc + 1) (IntMap.insert g c opens) caps c
      Close g   -> go (pc + 1) opens
                     (remember g (opens IntMap.! g) c caps) c
      _         -> caps
-- ANCHOR_END: straight

-- ANCHOR: measure
-- 🔢 For measuring: only the ends, counted (forward DFA only).
-- 測るため：終わりだけを数える（前向きの DFA だけ）
dfaCountEnds :: Regex -> Text -> Maybe Int
dfaCountEnds re t = do
  d <- dfaFor re
  either (const Nothing) Just $ runST $ do
    fm <- newMachine (forward d) True
    let go b n = forwardEnd fm t b >>= \case
          Left ()        -> pure (Left ())
          Right Nothing  -> pure (Right n)
          Right (Just e) -> go e (n + 1)
    go 0 (0 :: Int)

-- 📏 For measuring: the spans, with both DFAs, but no Match.
-- 測るため：範囲を、2つの DFA で。Match は作らない
dfaSpans :: Regex -> Text -> Maybe Int
dfaSpans re t = do
  d <- dfaFor re
  either (const Nothing) Just $ runST $ do
    fm <- newMachine (forward d) True
    bm <- newMachine (backward d) False
    let go b n = forwardEnd fm t b >>= \case
          Left ()        -> pure (Left ())
          Right Nothing  -> pure (Right n)
          Right (Just e) -> backwardStart bm t b e >>= \case
            Left ()  -> pure (Left ())
            Right s  -> go e (n + min 1 (e - s))
    go 0 (0 :: Int)

-- 🗄️ For GHCi: how many states the two DFAs made, for all the matches
--    in the text.
-- GHCi で試す用：文字列の一致を全部探すあいだに、2つの DFA が作った
-- 状態の数
dfaStates :: Regex -> Text -> Maybe (Int, Int)
dfaStates re t = do
  d <- dfaFor re
  pure $ runST $ do
    fm <- newMachine (forward d) True
    bm <- newMachine (backward d) False
    let go b = forwardEnd fm t b >>= \case
          Right (Just e) -> backwardStart bm t b e >> go e
          _              -> pure ()
    go 0
    (,) <$> readSTRef (count fm) <*> readSTRef (count bm)
-- ANCHOR_END: measure
-- ANCHOR_END: all
