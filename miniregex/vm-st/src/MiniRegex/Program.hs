-- ANCHOR: all
-- | The instructions of the virtual machine (VM), and a program: a row
--   of them, numbered from 0. A label is where an instruction is: a
--   name while the compiler is writing the program, and a number (an
--   address) when it is done.
-- 仮想機械（VM）の命令と、プログラム：0 から番号を振った命令の列。
-- ラベルは命令の場所。コンパイラーが書いているあいだは名前、書き終えたら
-- 番号（番地）
module MiniRegex.Program
  ( Inst (..)
  , Mark
  , Program
  , programOf
  , instructions
  ) where

-- ANCHOR: imports
import Data.Array (Array, elems, listArray)

import MiniRegex.Syntax (Anchor, Atom)
-- ANCHOR_END: imports

-- ANCHOR: inst
-- 📜 One instruction. `l` is the type of a label.
-- 命令1つ。l はラベルの型
data Inst l
  -- ANCHOR: basic
  -- Take one character that fits the atom (a, ., [a-z], \d ...).
  -- アトムに合う1文字を食べる
  = Take Atom
  -- A place that takes no character (^, $, \b ...).
  -- 文字を食べない場所の印
  | Assert Anchor
  -- Try the first label; if everything after it fails, the second.
  -- 1つ目のラベルを試す。その先が全部だめなら、2つ目
  | Split l l
  -- Go on at the label.
  -- ラベルへ飛ぶ
  | Jmp l
  -- Group n opens here / closes here.
  -- n 番のグループが、ここで開く・ここで閉じる
  | Open Int
  | Close Int
  -- \1, and (?i)\1: the text that group n found, again.
  -- \1 と (?i)\1：n 番のグループが見つけた文字列を、もう一度
  | Ref Int
  | RefNoCase Int
  -- \K: the match that is told starts here.
  -- \K：知らせる一致は、ここから始まる
  | Keep
  -- The whole pattern matched.
  -- パターン全体が合った
  | Match
  -- ANCHOR_END: basic
  -- ANCHOR: marks
  -- Write down where we are, in mark m.
  -- いまの場所を、印 m に書いておく
  | SetMark Mark
  -- If we have not moved since mark m, go to the label (a repeat
  -- that took nothing does not go round again).
  -- 印 m から進んでいなければ、ラベルへ（何も食べなかったくり返しは、
  -- もう回らない）
  | IfStill Mark l
  -- ANCHOR_END: marks
  -- ANCHOR: fences
  -- Put a fence on the stack (an atomic group, a positive look).
  -- スタックに区切りを積む（アトミックグループ、肯定の「見る」もの）
  | Fence
  -- Put a fence that says: if all inside fails, go on at the label
  -- (a negative look).
  -- 「中が全部だめなら、ラベルから先へ」という区切りを積む（否定の
  -- 「見る」もの）
  | FenceNot l
  -- The inside worked. Throw away the other ways to try, down to the
  -- fence and the fence itself, and then: go on (Cut), go on from
  -- where the fence was put (CutBack), or fail (CutFail).
  -- 中がうまくいった。区切りまでの、ほかの試し方と区切りを捨てて、
  -- 先へ進む（Cut）、区切りを積んだ場所から先へ進む（CutBack）、
  -- 失敗する（CutFail）
  | Cut
  | CutBack
  | CutFail
  -- ANCHOR_END: fences
  -- ANCHOR: behind
  -- Step back n characters (for a lookbehind).
  -- n 文字下がる（後読みのため）
  | Back Int
  -- Fail unless we are just where mark m is.
  -- 印 m のちょうどその場所にいなければ、失敗
  | AtMark Mark
  -- ANCHOR_END: behind
  deriving (Show, Eq, Functor)

-- 🔖 The number of a mark: a place the program writes down.
-- 印の番号：プログラムが書き留めておく場所
type Mark = Int
-- ANCHOR_END: inst

-- ANCHOR: program
-- 🗂️ A program: instructions with addresses from 0; each label is an
--    address. An array, so that a jump costs the same, however far.
-- プログラム：0 から番地の付いた命令。ラベルは番地。配列なので、
-- どれだけ遠くへ飛んでも、同じ手間
type Program = Array Int (Inst Int)

-- 🗂️ Make a program from the instructions, in order.
-- 命令を順に並べて、プログラムにする
programOf :: [Inst Int] -> Program
programOf is = listArray (0, length is - 1) is

-- 📋 The instructions, in order.
-- 命令を順に
instructions :: Program -> [Inst Int]
instructions = elems
-- ANCHOR_END: program
-- ANCHOR_END: all
