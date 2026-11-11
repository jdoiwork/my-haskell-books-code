{-# LANGUAGE OverloadedLabels #-}
module Main (main) where

import Data.List (sort)
import qualified Data.Text as T
import Database.SQLite.Simple
import Test.Hspec
import Test.QuickCheck

import Guild
import MiniSql.Expr
import MiniSql.Query
import SqlAnswers

-- 🧪 A fresh roster in memory, with four adventurers.
-- メモリー上に、4人の名簿を用意する
withRoster :: (Connection -> IO a) -> IO a
withRoster action = withConnection ":memory:" $ \conn -> do
  setup conn
  insert conn "Lambda" "mage" 3
  insert conn "Python" "healer" 2
  insert conn "Rust" "knight" 6
  insert conn "Ruby" "mage" 5
  action conn

main :: IO ()
main = hspec $ do
  describe "compiling" $ do
    -- 値は ? の穴に。文には入らない
    it "SELECT" $
      compileSelect (strong 3) `shouldBe`
        ( "SELECT id, name, job, level FROM adventurers \
          \WHERE (level >= ?) ORDER BY name ASC"
        , [SQLInteger 3] )
    -- UPDATE の SET と WHERE
    it "UPDATE" $
      compileUpdate adventurers [#level .= #level .+ lit 1]
                    (#job .== lit ("mage" :: T.Text))
        `shouldBe`
        ( "UPDATE adventurers SET level = (level + ?) WHERE (job = ?)"
        , [SQLInteger 1, SQLText "mage"] )
    -- 人数も ? の穴に。値は文に入らない
    it "the limit is a hole too" $
      compileSelect (mages 2) `shouldBe`
        ( "SELECT id, name, job, level FROM adventurers \
          \WHERE (job = ?) ORDER BY level DESC LIMIT ?"
        , [SQLText "mage", SQLInteger 2] )
  describe "running on SQLite" $ do
    -- 負の人数は 0 人（SQLite の「上限なし」にしない）
    it "a negative limit gives no rows" $ withRoster $ \conn -> do
      found <- runSelect conn (mages (-1))
      found `shouldBe` []
    -- レベル 3 以上を、名前の順に
    it "strong" $ withRoster $ \conn -> do
      found <- runSelect conn (strong 3)
      map name found `shouldBe` ["Lambda", "Ruby", "Rust"]
    -- 魔法使いのレベルを上げる
    it "levelUpMages" $ withRoster $ \conn -> do
      changed <- levelUpMages conn
      found <- runSelect conn (mages 5)
      (changed, map level found) `shouldBe` (2, [6, 4])
    -- いたずらの名前は、ただの文字列として扱われる
    it "is safe from SQL injection" $ withRoster $ \conn -> do
      removed <- retire conn "x' OR '1'='1"
      left <- runSelect conn (strong 0)
      (removed, length left) `shouldBe` (0, 4)
  -- ANCHOR: model
  describe "model" $
    -- SQL で絞っても、Haskell の filter で絞っても、同じ名前が残る
    it "strong n agrees with filter" $ property $ \levels n ->
      ioProperty $ withConnection ":memory:" $ \conn -> do
        setup conn
        let roster = zip [T.pack ("hero" ++ show i) | i <- [1 :: Int ..]]
                         (levels :: [Int])
        mapM_ (\(who, lv) -> insert conn who "mage" lv) roster
        found <- runSelect conn (strong n)
        let expected = sort [ who | (who, lv) <- roster, lv >= n ]
        pure (map name found === expected)
  -- ANCHOR_END: model
  describe "answers" $
    -- ステップ2：レベル 3〜5
    it "middleLevels" $ withRoster $ \conn -> do
      found <- runSelect conn middleLevels
      map name found `shouldBe` ["Lambda", "Ruby"]
