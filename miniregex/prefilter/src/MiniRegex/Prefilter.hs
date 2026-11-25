-- ANCHOR: all
-- | Look before the VM runs. From the program, find out where a match
--   can start (only at the first place, with this text, or with one of
--   these characters) and which character it surely needs. Then only
--   the places that pass are handed to the VM.
-- VM を動かす前に見る。プログラムから、一致がどこで始まりうるか
-- （最初の場所だけ、この文字列で、この文字のどれかで）と、必ず要る
-- 文字を調べる。VM には、それに合う場所だけを渡す
module MiniRegex.Prefilter
  ( Prefilter (..)
  , Start (..)
  , Finder (..)
  , prefilterOf
  , noPrefilter
  , startOf
  , literalPrefix
  , firstAtoms
  , neededChars
  , places
  , textPlaces
  , kmpPlaces
  , kmpTable
  ) where

-- ANCHOR: imports
import Data.Array (Array, bounds, listArray, (!))
import Data.IntSet qualified as IntSet
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import MiniRegex.Cursor
import MiniRegex.Program
import MiniRegex.Rules (fitsChar)
import MiniRegex.Syntax (Anchor (..), Atom (..))
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
  deriving (Show, Eq)

-- 🔎 How to look for a Prefix: Text's own search, or KMP.
-- Prefix の探し方：Text の検索か、KMP か
data Finder = ByText | ByKmp
  deriving (Show, Eq)

-- 🧹 What we know before running the VM.
-- VM を動かす前に、わかっていること
data Prefilter = Prefilter
  { start  :: Start
  , needed :: [Char]  -- every match needs them / どの一致にも要る文字
  , finder :: Finder
  }
  deriving (Show, Eq)

-- 🧹 Look at the program, once, when the regex is compiled.
-- 正規表現をコンパイルするときに、1度だけプログラムを見る
prefilterOf :: Program -> Prefilter
prefilterOf prog = Prefilter
  { start  = startOf prog
  , needed = nub (neededChars prog)
  , finder = ByText
  }

-- 🐢 Knowing nothing: try every place, as the chapters before did.
-- 何も知らない：前の章までと同じく、全部の場所を試す
noPrefilter :: Prefilter
noPrefilter = Prefilter Anywhere [] ByText
-- ANCHOR_END: types

-- ANCHOR: start
-- 🚦 Where a match can start: a prefix of two characters or more, or
--    else the first characters.
-- 一致がどこで始まりうるか。2文字以上の文字列で始まるなら、それ。
-- そうでなければ、最初の文字
startOf :: Program -> Start
startOf prog
  | T.length prefix >= 2 = Prefix prefix
  | otherwise            = firstAtoms prog
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
-- 🧷 The plain characters that every way from 0 to a match takes, in
--    the order they are taken. For each instruction, what every way
--    from it takes: at a split, only what both ways take. The loops
--    make it go round, so start from "anything" (Nothing: no way to a
--    match is known yet) and narrow down until nothing changes. Each
--    round makes every answer again, so a long program takes many
--    rounds of long lists: above neededLimit instructions, say
--    nothing (fewer is safe).
-- 0 番から match までの、どの道でも食べる、ただの文字。食べる順に。
-- 命令ごとに「そこからのどの道でも食べるもの」を求める。split では、
-- 両方の道が食べるものだけ。輪があるので、「何でも」（Nothing：match
-- へ行く道が、まだ見つかっていない）から始めて、変わらなくなるまで
-- 絞る。回るたびに全部の答えを作り直すので、長いプログラムでは、長い
-- リストを何回も作る。neededLimit を超える命令の数なら、何も言わない
-- （少なめに言うのは安全）
neededChars :: Program -> [Char]
neededChars prog
  | hi - lo + 1 > neededLimit = []
  | otherwise                 = fromMaybe [] (settle start0 ! 0)
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
          Take (Lit c) -> (c :) <$> after
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
    -- what both ways take (Nothing: that way never gets to a match)
    -- 両方の道が食べるもの（Nothing：その道は match に着かない）
    both Nothing ys         = ys
    both xs Nothing         = xs
    both (Just xs) (Just ys) = Just [x | x <- xs, x `elem` ys]

-- 📏 Up to how many instructions to look for the needed characters
--    ((?:a|b){1000}c, 3,000 instructions, took 11 seconds).
-- 必ず要る文字を、命令がいくつまでのプログラムで調べるか
-- （3,000 命令の (?:a|b){1000}c は 11 秒かかった）
neededLimit :: Int
neededLimit = 1000
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
      FirstChar as' -> filter (startsWithOneOf as') (onwards c)
  where
    startsWithOneOf as' d = case step d of
      Just (x, _) -> any (`fitsChar` x) as'
      Nothing     -> False
-- ANCHOR_END: places

-- ANCHOR: textplaces
-- 🔦 The places where t starts, with Text's own search (breakOn). The
--    next search starts one character later: t may overlap itself.
-- t が始まる場所を、Text の検索（breakOn）で。次の検索は、1文字先から。
-- t は、自分と重なることがあるから
textPlaces :: Text -> Cursor -> [Cursor]
textPlaces t c
  -- the empty t starts everywhere / 空の t は、どこでも始まる
  | T.null t = onwards c
textPlaces t c = case T.breakOn t (rest c) of
  (_, found) | T.null found -> []
  (skipped, found) ->
    let here = c { at     = at c + T.length skipped
                 , before = lastOf skipped
                 , rest   = found }
    in here : maybe [] (textPlaces t . snd) (step here)
  where
    lastOf s
      | T.null s  = before c
      | otherwise = Just (T.last s)
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
kmpPlaces t c
  -- the empty t starts everywhere / 空の t は、どこでも始まる
  | T.null t  = onwards c
  | otherwise = [ d | (d, k) <- zip cursors (drop (m - 1) states), k == m ]
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
