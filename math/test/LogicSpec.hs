module LogicSpec (spec, genHero, genNoKey) where

import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import LogicAnswers
import Math.Logic

-- 🎲 Random heroes for the gate. / 扉の前の、でたらめな冒険者
genHero :: Gen Hero
genHero = Hero <$> choose (1, 20) <*> arbitrary <*> elements [minBound ..]

-- ANCHOR: genNoKey
-- 🎲 Heroes of level 10 or more without a key:
--    the ones who must pick the lock.
-- 鍵を持たない、レベル 10 以上の冒険者（錠前を開けるしかない人）
genNoKey :: Gen Hero
genNoKey = Hero <$> choose (10, 20) <*> pure False <*> elements [minBound ..]
-- ANCHOR_END: genNoKey

spec :: Spec
spec = do
  -- ANCHOR: spec
  -- ド・モルガンの法則：4つの場合すべてで同じ
  it "De Morgan's laws hold in every case" $ do
    sameForAll (\a b -> not (a && b)) (\a b -> not a || not b)
      `shouldBe` True
    sameForAll (\a b -> not (a || b)) (\a b -> not a && not b)
      `shouldBe` True
  -- 追い返される人は、ちょうど入れない人
  it "turnedAway is exactly not canEnter" $
    forAll genHero $ \h -> turnedAway h == not (canEnter h)
  -- 下書きは、どこかでまちがえる
  it "the draft gets someone wrong" $
    expectFailure $ forAll genHero $ \h ->
      turnedAwayDraft h == not (canEnter h)
  -- 「すべて」の否定は「どれかが、そうでない」
  it "not (all p) is any (not . p)" $
    property $ \xs ->
      not (all even xs) == any (not . even) (xs :: [Int])
  -- 足りないスキルと、パーティーのスキルは重ならない
  it "missing skills and party skills never overlap" $
    property $ \needed members ->
      let neededSet  = Set.fromList (needed :: [String])
          memberSets = map Set.fromList (members :: [[String]])
      in  Set.null (Set.intersection
                      (missingSkills neededSet memberSets)
                      (partySkills memberSets))
  -- ANCHOR_END: spec

  -- ANCHOR: rules
  -- 「ならば」を「ちょうどそのとき」と読んだ下書きは、どこかでまちがえる
  it "the draft key rule gets someone wrong" $
    expectFailure $ forAll genHero $ \h ->
      keyRuleDraft h == keyRuleOk h
  -- 鍵なしで入れたならば、盗賊（==> は QuickCheck の「ならば」）。
  -- genHero では鍵なしで入れる人がめったに出ず、QuickCheck が
  -- あきらめるので、鍵を持たないベテランだけを作る
  it "only a thief gets in without a key" $
    forAll genNoKey $ \h ->
      pickedLock h ==> job h == Thief
  -- Bool の (<=) は、implies と同じ決まり
  it "implies is (<=) on Bool" $
    sameForAll implies (<=) `shouldBe` True
  -- ANCHOR_END: rules

  -- 「トレーニング」
  describe "answers" $ do
    it "xor' is the same as (/=)" $
      sameForAll xor' (/=) `shouldBe` True
    it "essentialMembers finds the only healer" $ do
      let needed = Set.fromList ["heal", "fire"]
          party  = [ ("Lambda", Set.fromList ["fire", "sword"])
                   , ("Python", Set.fromList ["heal"])
                   , ("Rust",   Set.fromList ["fire", "shield"]) ]
      essentialMembers needed party `shouldBe` ["Python"]
