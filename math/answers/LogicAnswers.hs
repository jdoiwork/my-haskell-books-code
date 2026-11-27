-- ANCHOR: all
-- | Answers to the training of "Logic and sets".
-- 「論理と集合」のトレーニングの解答
module LogicAnswers
  ( xor'
  , essentialMembers
  ) where

import Data.Set (Set)

import Math.Logic

-- ANCHOR: xor
-- 🔀 Step 2: "one or the other, but not both".
-- ステップ2：「どちらか一方だけ」
xor' :: Bool -> Bool -> Bool
xor' a b = (a || b) && not (a && b)
-- ANCHOR_END: xor

-- ANCHOR: essential
-- 🌟 Step 3: the members the party cannot do without: take one out
--    (by place, not by name), and a needed skill goes missing that was
--    not missing before.
-- ステップ3：いないと困る人。その人を（名前ではなく、並びの場所で）
-- 1人だけ外すと、外す前には足りていた、要るスキルが欠ける
essentialMembers :: Set Skill -> [(String, Set Skill)] -> [String]
essentialMembers needed party =
  [ name
  | (i, (name, _)) <- numbered
  , let others = [ skills | (j, (_, skills)) <- numbered, j /= i ]
  , missingSkills needed others /= missingBefore
  ]
  where
    numbered      = zip [0 :: Int ..] party
    missingBefore = missingSkills needed (map snd party)
-- ANCHOR_END: essential
-- ANCHOR_END: all
