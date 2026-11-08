-- ANCHOR: all
-- | Conditions and sets: the dungeon gate and the party's skills.
-- 条件と集合：ダンジョンの扉と、パーティーのスキル
module Math.Logic
  ( Job (..)
  , Hero (..)
  , canEnter
  , turnedAway
  , turnedAwayDraft
  , truthTable
  , sameForAll
  , implies
  , keyRuleOk
  , keyRuleDraft
  , pickedLock
  , Skill
  , partySkills
  , missingSkills
  , sharedSkills
  ) where

-- ANCHOR: imports
import Data.Set (Set)
import qualified Data.Set as Set
-- ANCHOR_END: imports

-- ANCHOR: gate
-- 🧙 A hero at the gate of the dungeon.
-- ダンジョンの扉の前の冒険者
data Job = Warrior | Mage | Thief
  deriving (Show, Eq, Enum, Bounded)

data Hero = Hero
  { level  :: Int
  , hasKey :: Bool
  , job    :: Job
  }
  deriving (Show)

-- 🚪 Level 10 or more, AND (a key OR a thief, who can pick the lock).
-- レベル 10 以上、かつ（鍵を持っている、または盗賊）
canEnter :: Hero -> Bool
canEnter h = level h >= 10 && (hasKey h || job h == Thief)
-- ANCHOR_END: gate

-- ANCHOR: turnedAway
-- 🙅 The same rule, turned around: who is sent back?
--    not (A && (B || C))  ==  not A || (not B && not C)
-- 同じ決まりを裏返す：追い返されるのは誰？
turnedAway :: Hero -> Bool
turnedAway h = level h < 10 || (not (hasKey h) && job h /= Thief)

-- 🐛 A first draft that turned every && into || and stopped there.
-- 下書き：&& を || に変えただけで、中の || を変え忘れた
turnedAwayDraft :: Hero -> Bool
turnedAwayDraft h = level h < 10 || not (hasKey h) || job h /= Thief
-- ANCHOR_END: turnedAway

-- ANCHOR: truth
-- 📋 Every case of a rule with two conditions: (a, b, answer).
-- 2つの条件の決まりを、すべての場合で
truthTable :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
truthTable rule =
  [ (a, b, rule a b) | a <- [False, True], b <- [False, True] ]

-- ✅ Do two rules agree in all four cases? (Then they are the same rule.)
-- 2つの決まりは、4つの場合すべてで同じ？（なら、同じ決まり）
sameForAll :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
sameForAll rule1 rule2 =
  and [ rule1 a b == rule2 a b | a <- [False, True], b <- [False, True] ]
-- ANCHOR_END: truth

-- ANCHOR: implies
-- ➡️ "If a, then b" is broken only when a is true and b is false.
-- 「a ならば b」が破れるのは、a が真で b が偽のときだけ
implies :: Bool -> Bool -> Bool
implies a b = not a || b
-- ANCHOR_END: implies

-- ANCHOR: keyRule
-- 🔑 The guild's rule: "if a hero carries a key, the level is 10 or more".
--    It says nothing about heroes without a key, so they keep it.
-- ギルドの決まり：「鍵を持っているならば、レベル 10 以上」。
-- 鍵を持っていない人の話はしていないので、その人たちは決まりを守っている
keyRuleOk :: Hero -> Bool
keyRuleOk h = hasKey h `implies` (level h >= 10)

-- 🐛 A draft that read "if" in the everyday way: "exactly when".
-- 下書き：「ならば」を、ふだんの言葉の感覚で「ちょうどそのとき」と読んだ
keyRuleDraft :: Hero -> Bool
keyRuleDraft h = hasKey h == (level h >= 10)

-- 🗝 Who got in without a key? (They picked the lock.)
-- 鍵なしで入れた人（錠前を開けた人）
pickedLock :: Hero -> Bool
pickedLock h = canEnter h && not (hasKey h)
-- ANCHOR_END: keyRule

-- ANCHOR: sets
-- 🎒 Skills are names; a party has the skills of all its members.
-- スキルは名前。パーティーは、全員のスキルを持つ
type Skill = String

-- ∪ Everything someone in the party can do (the union).
-- パーティーの誰かができること（和集合）
partySkills :: [Set Skill] -> Set Skill
partySkills members = Set.unions members

-- ∖ What the quest needs, minus what the party can do (the difference).
-- 依頼に要るものから、パーティーができることを引く（差集合）
missingSkills :: Set Skill -> [Set Skill] -> Set Skill
missingSkills needed members =
  Set.difference needed (partySkills members)

-- ∩ What every member can do (the intersection).
-- 全員ができること（共通部分）
sharedSkills :: [Set Skill] -> Set Skill
sharedSkills []                = Set.empty
sharedSkills (first : members) = foldr Set.intersection first members
-- ANCHOR_END: sets
-- ANCHOR_END: all
