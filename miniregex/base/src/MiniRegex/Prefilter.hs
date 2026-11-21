-- ANCHOR: all
-- | Look before the VM runs. From the program, find out where a match
--   can start (only at the first place, with this text, or with one of
--   these characters) and which character it surely needs. Then only
--   the places that pass are handed to the VM. From the chapter on
--   learning from other lands: a text inside the pattern to step back
--   from, and patterns that are only texts.
-- VM を動かす前に見る。プログラムから、一致がどこで始まりうるか
-- （最初の場所だけ、この文字列で、この文字のどれかで）と、必ず要る
-- 文字を調べる。VM には、それに合う場所だけを渡す。よその国に学ぶ章
-- から：パターンの内側の文字列から戻る形と、文字列だけのパターン
module MiniRegex.Prefilter
  ( Prefilter (..)
  , Start (..)
  , Lead (..)
  , Finder (..)
  , prefilterOf
  , noPrefilter
  , startOf
  , innerOf
  , literalsOf
  , literalPrefix
  , firstAtoms
  , neededChars
  , places
  , textPlaces
  , charPlaces
  , stepBackFrom
  , kmpPlaces
  , kmpTable
  ) where

-- ANCHOR: imports
import Data.Array (Array, bounds, listArray, (!))
import Data.Array.Unboxed (UArray, accumArray)
import Data.Array.Unboxed qualified as U
import Data.List.NonEmpty qualified as NE
import Data.IntSet qualified as IntSet
import Data.List (delete, insert, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (lengthWord8)

import MiniRegex.Cursor
import MiniRegex.Program
import MiniRegex.Rules (fitsChar)
import MiniRegex.Syntax
  (Anchor (..), Atom (..), Branch (..), Pattern (..), Piece (..), once)
import MiniRegex.Unicode (caseVariants)
-- ANCHOR_END: imports

-- ANCHOR: types
-- 🚦 Where a match can start.
-- 一致が、どこで始まりうるか
data Start
  = Anywhere          -- we cannot tell / わからない
  | OnlyFirst         -- \A, or ^ without m / 最初の場所だけ
  | Prefix Text       -- with this text / この文字列で始まる
  | FirstChar [Atom]  -- with a character that fits one of these
                      -- 最初の文字が、このどれかに合う
  -- ANCHOR: newstarts
  | Inner [Lead] Text -- these leads, and then this text
                      -- この前置きのあとに、この文字列
  | Literals [Text]   -- only one of these texts, nothing else
                      -- この文字列のどれか。ほかには何もない
  -- ANCHOR_END: newstarts
  deriving (Show, Eq)

-- ANCHOR: lead
-- 👣 What comes before the inner text: one character that fits the
--    atom, or a run of one or more of them.
-- 内側の文字列の前に来るもの：アトムに合う1文字か、合う文字が1つ以上
-- 続くもの
data Lead
  = One Atom
  | Run Atom
  deriving (Show, Eq)
-- ANCHOR_END: lead

-- 🔎 How to look for a Prefix: Text's own search, or KMP. (EachText,
--    from the chapter on learning from other lands, is only to compare
--    with: for Literals, each text with Text's search, put in order.)
-- Prefix の探し方：Text の検索か、KMP か。（EachText は、よその国に
-- 学ぶ章から、くらべるためだけのもの：Literals を、文字列ごとに Text の
-- 検索で探して、並べ直す）
data Finder = ByText | ByKmp | EachText
  deriving (Show, Eq)

-- 🧹 What we know before running the VM.
-- VM を動かす前に、わかっていること
data Prefilter = Prefilter
  { start  :: Start
  , needed :: [Char]  -- every match needs them / どの一致にも要る文字
  , finder :: Finder
  }
  deriving (Show, Eq)

-- ANCHOR: prefilterof
-- 🧹 Look at the tree and the program, once, when the regex is
--    compiled (the tree from the chapter on learning from other lands:
--    to see a pattern that is only texts).
-- 正規表現をコンパイルするときに、1度だけ木とプログラムを見る（木は
-- よその国に学ぶ章から：文字列だけのパターンを見つけるため）
prefilterOf :: Pattern -> Program -> Prefilter
prefilterOf tr prog = Prefilter
  { start  = maybe (startOf prog) Literals (literalsOf tr)
  , needed = nub (neededChars prog)
  , finder = ByText
  }
-- ANCHOR_END: prefilterof

-- 🐢 Knowing nothing: try every place, as the chapters before did.
-- 何も知らない：前の章までと同じく、全部の場所を試す
noPrefilter :: Prefilter
noPrefilter = Prefilter Anywhere [] ByText
-- ANCHOR_END: types

-- ANCHOR: start
-- 🚦 Where a match can start: a prefix of two characters or more; or
--    else a text inside, after leads we can step back over; or else
--    the first characters.
-- 一致がどこで始まりうるか。2文字以上の文字列で始まるなら、それ。
-- そうでなければ、戻れる前置きのあとの、内側の文字列。それもなければ、
-- 最初の文字
startOf :: Program -> Start
startOf prog
  | T.length prefix >= 2         = Prefix prefix
  | Just (ls, t) <- innerOf prog = Inner ls t
  | otherwise                    = firstAtoms prog
  where
    prefix = literalPrefix prog

-- 🔤 The characters every match starts with: follow the program from
--    0 while it takes plain characters (the instructions that take no
--    character and do not move are passed over).
-- どの一致も、この文字列で始まる。プログラムを 0 番からたどり、ただの
-- 文字を食べるあいだ集める（文字を食べず、動きもしない命令は、とばす）
literalPrefix :: Program -> Text
literalPrefix prog = T.pack (go 0)
  where
    go pc = case prog ! pc of
      Take (Lit c) -> c : go (pc + 1)
      Open _       -> go (pc + 1)
      Close _      -> go (pc + 1)
      SetMark _    -> go (pc + 1)
      _            -> []
-- ANCHOR_END: start

-- ANCHOR: inner
-- 🔙 Like Rust's "reverse inner", in a small way: from 0, characters
--    that each take one character (One), maybe with a run at the very
--    left (Run: a take and a takeall of the same atom), and then plain
--    characters. The run must not take the first of those characters:
--    then stepping back from the text stops where the run starts, and
--    no character is stepped back over twice.
-- Rust の「reverse inner」を小さく：0 番から、1文字ずつ食べるもの
-- （One）が並び、いちばん左には続く文字（Run：同じアトムの take と
-- takeall）があってもよく、そのあとに、ただの文字が来る。Run は、その
-- 文字列の最初の文字を食べてはいけない。そうすれば、文字列から戻るのは
-- Run の始まりで止まり、同じ文字を2度戻ることはない
innerOf :: Program -> Maybe ([Lead], Text)
innerOf prog = leads (skip 0) []
  where
    -- instructions that take no character and do not move
    -- 文字を食べず、動きもしない命令は、とばす
    skip pc = case prog ! pc of
      Open _    -> skip (pc + 1)
      Close _   -> skip (pc + 1)
      SetMark _ -> skip (pc + 1)
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
-- ANCHOR_END: inner

-- ANCHOR: literals
-- 📜 Like Rust's first way: is the whole pattern only a choice of plain
--    texts? (With (?i), each character becomes its case variants:
--    (?i)perl is 16 texts. At most 64 texts.) In the order of the
--    branches: at one place, the first text there is the match, as the
--    VM would find it.
-- Rust の最初の手：パターン全体が、ただの文字列のどれか、というだけか？
-- （(?i) なら、1文字ずつ大文字・小文字に開く。(?i)perl は 16 個。多くて
-- 64 個。）枝の順に並べる。1つの場所では、そこにある最初の文字列が一致に
-- なる。VM が見つけるのと同じ
literalsOf :: Pattern -> Maybe [Text]
literalsOf (Pattern bs) = do
  ws <- concat <$> traverse texts (NE.toList bs)
  if length ws <= 64 && not (any T.null ws) then Just ws else Nothing
  where
    texts (Branch ps) = map T.pack . sequence <$> traverse piece ps
    piece (Quantified a q) | q == once = case a of
      Lit c          -> Just [c]
      NoCase (Lit c) -> Just (caseVariants c)
      _              -> Nothing
    piece _ = Nothing
-- ANCHOR_END: literals

-- ANCHOR: first
-- 🌱 Follow every way from 0 through the instructions that take no
--    character, and gather the first take of each. If some way gets to
--    a match without taking a character (or we cannot see through an
--    instruction), the match can start anywhere.
-- 0 番から、文字を食べない命令をたどって、どの道でも最初の take を
-- 集める。文字を食べずに match に着く道がある（か、見通せない命令が
-- ある）なら、一致はどこででも始まりうる
firstAtoms :: Program -> Start
firstAtoms prog = case walk [0] IntSet.empty [] False of
  Nothing                    -> Anywhere
  Just ([], True)            -> OnlyFirst
  Just (atoms, False)        -> FirstChar (nub (reverse atoms))
  Just (_, True)             -> Anywhere  -- ^a|b: both kinds
  where
    walk [] _ atoms anchored = Just (atoms, anchored)
    walk (pc : todo) seen atoms anchored
      | IntSet.member pc seen = walk todo seen atoms anchored
      | otherwise = case prog ! pc of
          Take a            -> go todo (a : atoms) anchored
          -- none, or some: the atom, or what comes next
          -- 0個か、いくつか：そのアトムか、次のもの
          TakeAll a         -> walk (pc + 1 : todo) seen' (a : atoms) anchored
          Assert TextStart  -> go todo atoms True
          Assert _          -> on [pc + 1]
          Split a b         -> on [a, b]
          Jmp a             -> on [a]
          IfStill _ a       -> on [a, pc + 1]
          -- a negative look: the match goes on at `a`
          -- 否定の「見る」もの：一致は a から先へ続く
          FenceNot a        -> on [a]
          CutFail           -> go todo atoms anchored
          Open _            -> on [pc + 1]
          Close _           -> on [pc + 1]
          Keep              -> on [pc + 1]
          SetMark _         -> on [pc + 1]
          Fence             -> on [pc + 1]
          Cut               -> on [pc + 1]
          CutBack           -> on [pc + 1]
          -- match, ref, back, calls: we cannot tell
          -- match・ref・back・呼び出し：わからない
          _                 -> Nothing
      where
        seen' = IntSet.insert pc seen
        go more = walk more seen'
        on next = walk (next ++ todo) seen' atoms anchored
-- ANCHOR_END: first

-- ANCHOR: needed
-- 🧷 The plain characters that every way from 0 to a match takes,
--    kept sorted (from the chapter on measuring: in the order they
--    were taken, the same characters in another order looked like a
--    change, and the narrowing could go round for ever). For each
--    instruction, what every way
--    from it takes: at a split, only what both ways take. The loops
--    make it go round, so start from "anything" (Nothing: no way to a
--    match is known yet) and narrow down until nothing changes.
-- 0 番から match までの、どの道でも食べる、ただの文字。並べ替えて
-- 持つ（測って比べる章から。食べる順に持つと、同じ文字が順番だけ
-- ちがうのを「変わった」と見て、絞り込みが終わらないことがあった）。
-- 命令ごとに「そこからのどの道でも食べるもの」を求める。split では、
-- 両方の道が食べるものだけ。輪があるので、「何でも」（Nothing：match
-- へ行く道が、まだ見つかっていない）から始めて、変わらなくなるまで
-- 絞る
neededChars :: Program -> [Char]
neededChars prog = fromMaybe [] (settle start0 ! 0)
  where
    (lo, hi) = bounds prog
    start0   = listArray (lo, hi) (replicate (hi - lo + 1) Nothing)
    settle :: Array Int (Maybe [Char]) -> Array Int (Maybe [Char])
    settle need
      | next == need = need
      | otherwise    = settle next
      where
        next = listArray (lo, hi) [rule pc | pc <- [lo .. hi]]
        rule pc = case prog ! pc of
          Take (Lit c) -> insert c <$> after
          Match        -> Just []
          Split a b    -> both (need ! a) (need ! b)
          Jmp a        -> need ! a
          IfStill _ a  -> both (need ! a) after
          FenceNot a   -> need ! a
          CutFail      -> Nothing          -- no way on / 先がない
          Back _       -> Just []          -- may be before / 前かも
          Return       -> Just []          -- comes back anywhere
                                           -- どこへ戻るかわからない
          _            -> after
          where
            after = need ! (pc + 1)
    -- ANCHOR: common
    -- what both ways take (Nothing: that way never gets to a match)
    -- 両方の道が食べるもの（Nothing：その道は match に着かない）
    both Nothing ys         = ys
    both xs Nothing         = xs
    both (Just xs) (Just ys) = Just (common xs ys)
    -- each character of ys is used up once: so the answer is never
    -- longer than ys; and both are sorted, so the answer is too, and
    -- the narrowing comes to an end
    -- ys の文字は、1つにつき1回だけ使う。だから答えは ys より長くならない。
    -- 両方が並べ替えてあるので、答えも並ぶ。絞り込みは終わる
    common [] _ = []
    common (x : xs) ys
      | x `elem` ys = x : common xs (delete x ys)
      | otherwise   = common xs ys
    -- ANCHOR_END: common
-- ANCHOR_END: needed

-- ANCHOR: places
-- 🗺️ The places worth trying, from this cursor on, in order. If a
--    needed character is not ahead, none is.
-- このしおりから先で、試す値打ちのある場所を順に。必ず要る文字が
-- 1つでも、この先になければ、1つもない
places :: Prefilter -> Cursor -> [Cursor]
places pf c
  | not (all (`T.elem` rest c) (needed pf)) = []
  | otherwise = case start pf of
      Anywhere      -> onwards c
      OnlyFirst     -> [c | at c == 0]
      Prefix t      -> case finder pf of
        ByText -> textPlaces t c
        ByKmp  -> kmpPlaces t c
        _      -> textPlaces t c
      FirstChar as'
        | Just cs <- plainChars as' -> charPlaces cs c
        | otherwise -> filter (startsWithOneOf as') (onwards c)
      Inner ls t    -> stepBackFrom c ls (textPlaces t c)
      Literals ws
        | finder pf == EachText -> eachTextPlaces (nub ws) c
        | otherwise -> charPlaces (nub (mapMaybe firstChar ws)) c
  where
    startsWithOneOf as' d = case step d of
      Just (x, _) -> any (`fitsChar` x) as'
      Nothing     -> False
    firstChar = fmap fst . T.uncons
    -- the characters, if every atom is a plain one (or one with (?i))
    -- どのアトムも、ただの文字（か (?i) の文字）なら、その文字
    plainChars = fmap concat . traverse plain
    plain (Lit x)          = Just [x]
    plain (NoCase (Lit x)) = Just (caseVariants x)
    plain _                = Nothing
-- ANCHOR_END: places

-- ANCHOR: stepback
-- 🔙 From each place where the inner text starts, step back over the
--    leads, from the right; never to before `from`, where the search
--    began. A place that the leads do not fit is dropped.
-- 内側の文字列が始まる場所ごとに、前置きを右から戻る。探し始めた from
-- より前には戻らない。前置きが合わない場所は、捨てる
stepBackFrom :: Cursor -> [Lead] -> [Cursor] -> [Cursor]
stepBackFrom from ls = mapMaybe (backOver (reverse ls))
  where
    backOver [] d             = Just d
    backOver (One a : more) d = backOne a d >>= backOver more
    backOver (Run a : more) d = backOne a d >>= backOver more . backAll a
    -- one character that fits a, if we are still after `from`
    -- a に合う1文字。まだ from より後ろなら
    backOne a d = case before d of
      Just x | at d > at from, fitsChar a x -> back 1 d
      _                                    -> Nothing
    backAll a d = maybe d (backAll a) (backOne a d)
-- ANCHOR_END: stepback

-- ANCHOR: charplaces
-- 🔦 The places where one of these characters is: one run of Text's
--    `break` to the next, with a table for ASCII (not a walk with `step`
--    and `elem`, a character at a time).
-- この文字のどれかがある場所。Text の break を1回走らせて次へ。ASCII は
-- 表で引く（step と elem で1文字ずつ歩くのではなく）
charPlaces :: [Char] -> Cursor -> [Cursor]
charPlaces cs = go
  where
    go d = case T.break isOne (rest d) of
      (_, r) | T.null r -> []
      (skipped, _) ->
        let here = jumpTo (T.length skipped) (lengthWord8 skipped) d
        in here : maybe [] (go . snd) (step here)
    table = accumArray (\_ b -> b) False (0, 127)
              [(fromEnum x, True) | x <- cs, x < '\x80'] :: UArray Int Bool
    others = filter (>= '\x80') cs
    isOne x
      | x < '\x80' = table U.! fromEnum x
      | otherwise  = x `elem` others
-- ANCHOR_END: charplaces

-- ANCHOR: eachtext
-- 🐌 Each text with Text's search, and the places put in order. To
--    know which place comes first, every search must find its next
--    place: a text that is not there is looked for to the very end,
--    again at each match.
-- 文字列ごとに Text の検索で探し、場所を並べ直す。どれが先かを知るには、
-- どの検索も次の場所を見つけないといけない。ない文字列は、一致のたびに、
-- 最後まで探し直す
eachTextPlaces :: [Text] -> Cursor -> [Cursor]
eachTextPlaces ws c = foldr merge [] [textPlaces w c | w <- ws]
  where
    merge xs [] = xs
    merge [] ys = ys
    merge (x : xs) (y : ys)
      | at x < at y = x : merge xs (y : ys)
      | at x > at y = y : merge (x : xs) ys
      | otherwise   = x : merge xs ys
-- ANCHOR_END: eachtext

-- ANCHOR: textplaces
-- 🔦 The places where t starts, with Text's own search (breakOn). The
--    next search starts one character later: t may overlap itself.
-- t が始まる場所を、Text の検索（breakOn）で。次の検索は、1文字先から。
-- t は、自分と重なることがあるから
textPlaces :: Text -> Cursor -> [Cursor]
textPlaces t c = case T.breakOn t (rest c) of
  (_, found) | T.null found -> []
  (skipped, _) ->
    let here = jumpTo (T.length skipped) (lengthWord8 skipped) c
    in here : maybe [] (textPlaces t . snd) (step here)
-- ANCHOR_END: textplaces

-- ANCHOR: kmp
-- 🧮 KMP's table for t, as in the chapter on string algorithms: for
--    each prefix, the length of its longest border.
-- t の KMP の表。文字列のアルゴリズムの章と同じ：先頭部分ごとに、
-- いちばん長い境界の長さ
kmpTable :: Text -> Array Int Int
kmpTable t = table
  where
    m     = T.length t
    p     = listArray (0, m - 1) (T.unpack t) :: Array Int Char
    table = listArray (0, m - 1)
              (0 : [border i (table ! (i - 1)) | i <- [1 .. m - 1]])
    border i k
      | p ! i == p ! k = k + 1
      | k == 0         = 0
      | otherwise      = border i (table ! (k - 1))

-- 🔎 The places where t starts, with KMP: walk the text once, keeping
--    how much of t matches. The cursor where a match of t starts is
--    m - 1 characters behind the one where it ends, so walk the
--    cursors and the states side by side, m - 1 apart.
-- t が始まる場所を、KMP で。文字列を1回だけたどり、t のどこまで
-- 合っているかを持ち回る。t が始まるしおりは、終わるしおりより
-- m − 1 文字前なので、しおりと状態を m − 1 ずらして並べて歩く
kmpPlaces :: Text -> Cursor -> [Cursor]
kmpPlaces t c =
  [ d | (d, k) <- zip cursors (drop (m - 1) states), k == m ]
  where
    m       = T.length t
    p       = listArray (0, m - 1) (T.unpack t) :: Array Int Char
    table   = kmpTable t
    cursors = onwards c
    states  = drop 1 (scanl stepK 0 (T.unpack (rest c)))
    stepK k x
      | k == m     = stepK (table ! (m - 1)) x
      | p ! k == x = k + 1
      | k == 0     = 0
      | otherwise  = stepK (table ! (k - 1)) x
-- ANCHOR_END: kmp
-- ANCHOR_END: all
