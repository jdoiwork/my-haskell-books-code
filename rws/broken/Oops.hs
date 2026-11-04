-- | This file compiles, but -Wall warns: s2 is never used.
-- このファイルはコンパイルできるが、-Wall が「s2 を使っていない」と警告する
module Oops (threeRollsOops) where

import Dice

-- ANCHOR: oops
-- 😱 The same as threeRolls, with one typo: s1 is used twice.
-- threeRolls と同じものを、1文字まちがえて：s1 を2回使ってしまった
threeRollsOops :: Seed -> ([Int], Seed)
threeRollsOops s0 =
  let (a, s1) = rollDie s0
      (b, s2) = rollDie s1
      (c, s3) = rollDie s1
  in  ([a, b, c], s3)
-- ANCHOR_END: oops
