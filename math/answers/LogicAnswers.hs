-- ANCHOR: all
-- | Answers to the training of "Logic and sets".
-- 「論理と集合」のトレーニングの解答
module LogicAnswers
  ( xor'
  , essentialMembers
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Math.Logic

-- ANCHOR: xor
-- 🔀 Step 2: "one or the other, but not both".
-- ステップ2：「どちらか一方だけ」
xor' :: Bool -> Bool -> Bool
xor' a b = (a || b) && not (a && b)
-- ANCHOR_END: xor

-- ANCHOR: essential
-- 🌟 Step 3: the members the party cannot do without: take one out,
--    and some needed skill goes missing.
-- ステップ3：いないと困る人。その人を外すと、要るスキルが欠ける
essentialMembers :: Set Skill -> [(String, Set Skill)] -> [String]
essentialMembers needed party =
  [ name
  | (name, _) <- party
  , let others = [ skills | (other, skills) <- party, other /= name ]
  , not (Set.null (missingSkills needed others))
  ]
-- ANCHOR_END: essential
-- ANCHOR_END: all
