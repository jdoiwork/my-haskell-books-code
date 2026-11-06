module Main (main) where

import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState, execState, runState)
import Control.Monad.Writer.CPS (runWriter)
import Test.Hspec
import Test.QuickCheck

import Battle
import Config
import Dice
import Logged
import qualified MyState as My
import RwsAnswers
import StRef

main :: IO ()
main = hspec $ do
  describe "State" $ do
    -- 同じ種からは、いつも同じ目
    it "the same seed gives the same roll" $ property $ \s ->
      rollDie s == rollDie s
    -- 手で種を渡しても、State でも、同じ3つの目
    it "threeRolls by hand and with State agree" $ property $ \s ->
      threeRolls s == My.runState My.threeRollsS s
    -- バトルは、いつか、どちらかが倒れて終わる
    it "a fight always ends with someone down" $ property $ \s ->
      let (winner, arena) = runState fight (start (abs s))
      in  case winner of
            "hero" -> goblinHp arena <= 0
            _      -> heroHp arena <= 0
    -- 振っても、HP は変わらない。種だけが進む
    it "rolling only moves the seed" $
      execState (replicateRolls 3) (start 1) `shouldBe`
        (start 1) { seed = snd (threeRolls 1) }
  describe "Reader" $ do
    -- 手で渡しても、Reader でも、同じ報告
    it "by hand and with Reader agree" $ property $ \(Positive level) n ->
      let cfg = easy { difficulty = level }
      in  floorReportByHand cfg n == runReader (floorReport n) cfg
    -- local は、その部分の中だけ設定を変える
    it "local changes the config only inside" $
      runReader bossFloor easy `shouldBe`
        "Welcome, Lambda! Floor 10: goblin with 200 HP"
  describe "Writer" $ do
    -- 組で手で書いても、Writer でも、同じ記録
    it "by hand and with Writer agree" $
      runWriter (twoAttacks 30) `shouldBe`
        (14, ["hit for 7", "hit for 9"])
  describe "ST" $ do
    -- ST でも State でも、合計は sum と同じ
    it "sumST and sumState agree with sum" $ property $ \xs ->
      sumST xs == sum xs && sumState xs == sum xs
  describe "answers" $ do
    -- ステップ2：番号を配る
    it "labelAll numbers the names" $
      labelAll ["goblin", "slime", "goblin"] `shouldBe`
        ["goblin-1", "slime-2", "goblin-3"]
    -- ステップ3：一部分だけ、別の人として
    it "asPlayer changes the name inside" $
      runReader (asPlayer "Python" greeting) easy `shouldBe`
        "Welcome, Python!"
