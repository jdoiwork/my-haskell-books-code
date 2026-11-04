-- ANCHOR: all
-- ANCHOR: module
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🧪 Example tests of the roster, each on a fresh database.
-- 名簿の例のテスト。テストごとに、まっさらなデータベースで
module RosterSpec (spec, withRoster) where

-- The roster, and SQLite.
-- 名簿と SQLite
import Roster
import Database.SQLite.Simple

-- hspec.
-- hspec
import Test.Hspec
-- ANCHOR_END: module

-- ANCHOR: withRoster
-- 🧺 Open a new roster in memory, use it, and close it.
-- メモリー上に新しい名簿を開いて、使って、閉じる
withRoster :: (Connection -> IO a) -> IO a
withRoster action =
  withConnection ":memory:" $ \conn -> do
    createTable conn
    action conn
-- ANCHOR_END: withRoster

-- ANCHOR: members
-- 🧙 Adventurers for the tests.
-- テストに使う冒険者
lambda, python :: NewAdventurer
lambda = NewAdventurer { newName = "Lambda", newJob = "Mage",   newLevel = 3 }
python = NewAdventurer { newName = "Python", newJob = "Archer", newLevel = 5 }

-- 🚫 Is this error a broken rule of the table (such as UNIQUE)?
-- このエラーは、表の決まり（UNIQUE など）を破ったもの？
isConstraintError :: SQLError -> Bool
isConstraintError e = e.sqlError == ErrorConstraint
-- ANCHOR_END: members

-- ANCHOR: spec
spec :: Spec
spec = around withRoster $ do

  describe "a new roster" $ do
    -- 「空っぽから始まる」
    it "starts empty" $ \conn -> do
      everyone <- listAdventurers conn
      everyone `shouldBe` []

  describe "addAdventurer" $ do
    -- 「名簿に冒険者を足す」
    it "adds adventurers in the order they join" $ \conn -> do
      addAdventurer conn lambda
      addAdventurer conn python
      everyone <- listAdventurers conn
      map (.name) everyone `shouldBe` ["Lambda", "Python"]

    -- 「同じ名前は、2回は登録できない」
    it "rejects the same name twice" $ \conn -> do
      addAdventurer conn lambda
      addAdventurer conn lambda `shouldThrow` isConstraintError

  describe "levelUp" $ do
    -- 「レベルを1上げる」
    it "raises the level by one" $ \conn -> do
      addAdventurer conn lambda
      changed <- levelUp conn "Lambda"
      found   <- findAdventurer conn "Lambda"
      changed `shouldBe` 1
      fmap (.level) found `shouldBe` Just 4

    -- 「いない名前なら、何も変わらない」
    it "changes nothing for an unknown name" $ \conn -> do
      levelUp conn "Nobody" `shouldReturn` 0

  describe "removeAdventurer" $ do
    -- 「名簿から外す」
    it "removes the adventurer" $ \conn -> do
      addAdventurer conn lambda
      removeAdventurer conn "Lambda" `shouldReturn` 1
      listAdventurers conn `shouldReturn` []
-- ANCHOR_END: spec
-- ANCHOR_END: all
