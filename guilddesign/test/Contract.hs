-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 📜 The contract: what every store of the roster must do.
-- 契約：名簿の置き場なら、どれもこう動かなければならない
module Contract (rosterContract) where

import Guild.Roster
import Guild.Store
import Test.Hspec

-- ANCHOR: contract
-- 📜 The same tests for any store. Each test gets an empty store.
-- どの置き場にも同じテスト。テストごとに、空っぽの置き場を受け取る
rosterContract :: SpecWith RosterStore
rosterContract = do

    -- 「空っぽから始まる」
    it "starts empty" $ \store ->
      store.listAdventurers `shouldReturn` []

    -- 「登録すると、番号が付いて、登録した順に並ぶ」
    it "gives ids in the order they join" $ \store -> do
      store.addAdventurer lambda `shouldReturn` Right 1
      store.addAdventurer python `shouldReturn` Right 2
      everyone <- store.listAdventurers
      map (.name) everyone `shouldBe` ["Lambda", "Python"]

    -- 「同じ名前は、NameTaken で断る」
    it "rejects a taken name with NameTaken" $ \store -> do
      _ <- store.addAdventurer lambda
      store.addAdventurer lambda `shouldReturn` Left (NameTaken "Lambda")

    -- 「レベルを上げられるのは、いる人だけ」
    it "levels up only someone in the roster" $ \store -> do
      _ <- store.addAdventurer lambda
      store.levelUp "Lambda" `shouldReturn` True
      store.levelUp "Nobody" `shouldReturn` False
      found <- store.findAdventurer "Lambda"
      fmap (.level) found `shouldBe` Just 4

    -- 「外した人は、もう見つからない」
    it "forgets a removed adventurer" $ \store -> do
      _ <- store.addAdventurer lambda
      store.removeAdventurer "Lambda" `shouldReturn` True
      store.findAdventurer "Lambda" `shouldReturn` Nothing
  where
    lambda = NewAdventurer "Lambda" "Mage"   3
    python = NewAdventurer "Python" "Archer" 5
-- ANCHOR_END: contract
-- ANCHOR_END: all
