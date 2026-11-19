-- ANCHOR: all
-- ANCHOR: head
-- | The answers to the training: the design chapter's, and then the
--   chapter on switching engines.
-- トレーニングの解答：設計の章のものと、エンジンを乗り換える章のもの
module RegexAnswers
  ( colourTree
  , choiceTree
  , escape
  , scanSwitchingBy
  , possessLazy
  , compileLazy
  ) where

import Data.Array ((!))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (lengthWord8)

import MiniRegex.Compile
  (Line (..), apart, assemble, nextAtoms, possess, writeOut)
import MiniRegex.Cursor (Cursor, byte, whole)
import MiniRegex.Engine (Runner)
import MiniRegex.PikeST (scanPikeST)
import MiniRegex.Program
import MiniRegex.STVM (scanVMST)
import MiniRegex.Syntax
import MiniRegex.VM (Answer (..))
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

-- The training of the chapter on the cursor and the instructions
-- しおりと命令を磨く章のトレーニング

-- The chapter on switching engines
-- エンジンを乗り換える章

-- ANCHOR: budget
-- 💰 scanSwitching with a budget of k times the instructions for each
--    byte (scanSwitching is k = 2). With k = 0, the Pike VM from the
--    start.
-- 予算を、バイトごとに命令の数の k 倍にした scanSwitching（scanSwitching
-- は k = 2）。k = 0 なら、はじめから Pike VM
scanSwitchingBy :: Int -> Runner
scanSwitchingBy k prog limit accept tries = case tries of
  [] -> (Nothing, NotFound, limit)
  first : _
    | budget >= limit -> scanVMST prog limit accept tries
    | otherwise -> case scanVMST prog budget accept tries of
        (_, OutOfSteps, _) -> scanPikeST prog (limit - budget) accept tries
        (o, answer, left)  -> (o, answer, limit - budget + left)
    where
      budget = k * length (instructions prog) * (bytesLeft first + 1)

-- 📏 The bytes from the cursor to the end.
-- しおりから終わりまでのバイトの数
bytesLeft :: Cursor -> Int
bytesLeft c = lengthWord8 (whole c) - byte c
-- ANCHOR_END: budget

-- ANCHOR: lazy
-- 🔒 possess, for lazy loops too. Where giving back cannot help, it
--    does not matter which way is tried first: a+?b and a+b both take
--    every a before the b. So a lazy loop that is safe becomes the
--    same takeall.
-- 控えめな輪にも possess を。返しても役に立たない所では、どちらの道を先に
-- 試しても同じ：a+?b も a+b も、b の前の a を全部食べる。だから、安全な
-- 控えめな輪も、同じ takeall になる
possessLazy :: [Line] -> [Line]
possessLazy ls = go ls
  where
    prog = assemble ls
    places = IntMap.fromList (addresses 0 ls)
    addresses _ []              = []
    addresses n (Here x : more) = (x, n) : addresses n more
    addresses n (Do _ : more)   = addresses (n + 1) more

    -- a*?: split out, top / top: take a / split out, top / out:
    go (Do (Split o t) : Here t' : Do (Take a) : Do (Split o2 t2)
        : Here o' : more)
      | all (== t) [t', t2], all (== o) [o2, o'], safe a o =
          Here t : Do (TakeAll a) : Here o : go more
    -- the loop of a+?: top: take a / split out, top / out:
    go (Here t : Do (Take a) : Do (Split o t') : Here o' : more)
      | t == t', o == o', safe a o =
          Here t : Do (Take a) : Do (TakeAll a) : Here o : go more
    go (l : more) = l : go more
    go []         = []

    -- A match must not come next: a lazy loop would stop there with
    -- as few as it can, but takeall takes them all.
    -- match が次に来てはいけない。控えめな輪は、そこでなるべく少なく
    -- 止まるが、takeall は全部食べてしまう
    safe a out = case nextAtoms prog (places IntMap.! out) of
      Just bs -> all (apart a) bs && not (matchNext prog (places IntMap.! out))
      Nothing -> False

-- 🏁 Can a match come next from this address, taking no character?
-- この番地から、文字を食べずに match に着けるか
matchNext :: Program -> Int -> Bool
matchNext prog = go IntSet.empty
  where
    go seen pc
      | IntSet.member pc seen = False
      | otherwise = case prog ! pc of
          Match     -> True
          Split a b -> go seen' a || go seen' b
          Jmp a     -> go seen' a
          Open _    -> go seen' (pc + 1)
          Close _   -> go seen' (pc + 1)
          _         -> False
      where
        seen' = IntSet.insert pc seen

-- 🏗️ compileTree, with possessLazy after possess.
-- possess のあとに possessLazy をかける compileTree
compileLazy :: Pattern -> Program
compileLazy p = assemble (possessLazy (possess (writeOut p)))
-- ANCHOR_END: lazy
-- ANCHOR_END: all
