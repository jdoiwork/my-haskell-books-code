-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🐣 A first try: every test opens its own database by hand.
-- 最初の一歩：テストごとに、手でデータベースを開く
module FirstSpec (spec) where

import Roster
import Database.SQLite.Simple
import Test.Hspec
-- ANCHOR_END: imports

-- ANCHOR: spec
spec :: Spec
spec =
  describe "first try (without around)" $ do
    -- 「空っぽから始まる」
    it "starts empty" $
      withConnection ":memory:" $ \conn -> do
        createTable conn
        everyone <- listAdventurers conn
        everyone `shouldBe` []

    -- 「名簿に冒険者を足す」
    it "adds an adventurer" $
      withConnection ":memory:" $ \conn -> do
        createTable conn
        addAdventurer conn (NewAdventurer "Lambda" "Mage" 3)
        everyone <- listAdventurers conn
        map (.name) everyone `shouldBe` ["Lambda"]
-- ANCHOR_END: spec
-- ANCHOR_END: all
