module Main (main) where

-- ANCHOR: helpers
import Data.Foldable (toList)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import MiniMq.Broker
import MiniMq.Examples
import MiniMq.Routing
import MiniMq.Types
import MqAnswers

-- 🎲 Keys and patterns made of a few words, so that they meet often.
-- 少ない単語でできた札と模様（よく出会うように）
genKey, genPattern :: Gen RoutingKey
genKey     = RoutingKey <$> dotted ["quest", "dragon", "north"]
genPattern = RoutingKey <$> dotted ["quest", "dragon", "north", "*", "#"]

dotted :: [T.Text] -> Gen T.Text
dotted ws = do
  n <- choose (1, 4)
  T.intercalate "." <$> vectorOf n (elements ws)

-- 📬 Where a key goes, in the guild's post office.
-- ギルドの伝令所で、その札がどこへ行くか
sentTo :: ExchangeName -> RoutingKey -> [QueueName]
sentTo desk key = case surely (dryRun desk key (surely guildPost)) of
  Routed qs  -> toList qs
  Unroutable -> []

-- ➕ Add ".#" to a pattern: "quest.*" → "quest.*.#"
-- 模様の後ろに「.#」を足す
withHash :: RoutingKey -> RoutingKey
withHash (RoutingKey p) = RoutingKey (p <> ".#")
-- ANCHOR_END: helpers

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "the three desks" $ do
    -- notices は、結んだ全員に配る
    it "Fanout sends to everyone bound" $
      sentTo "notices" "anything" `shouldBe` ["dragon-team", "north-guard"]
    -- mail は、札がぴったり同じキューにだけ
    it "Direct sends only on an exact match" $ do
      sentTo "mail" "dragon-team" `shouldBe` ["dragon-team"]
      sentTo "mail" "dragon"      `shouldBe` []
    -- quests は、模様に合うキューに
    it "Topic sends by pattern" $ do
      sentTo "quests" "quest.dragon.north"
        `shouldBe` ["dragon-team", "north-guard", "archive"]
      sentTo "quests" "quest" `shouldBe` ["archive"]
  describe "topic patterns" $ do
    -- 「#」だけの模様は、Fanout と同じ
    it "a lone # matches every key, like Fanout" $
      forAll genKey $ \key -> matches Topic "#" key === matches Fanout "#" key
    -- 「*」も「#」もない模様は、Direct と同じ
    it "a pattern without * or # is the same as Direct" $
      forAll genKey $ \plain -> forAll genKey $ \key ->
        matches Topic plain key === matches Direct plain key
    -- 模様の後ろに「.#」を足しても、合っていたものは合ったまま
    it "adding .# to a pattern never loses a match" $
      forAll genPattern $ \p -> forAll genKey $ \key ->
        matches Topic p key ==> matches Topic (withHash p) key
    -- 「*」は1語ちょうど、「#」は 0 語以上
    it "* takes one word, # takes any number" $ do
      matches Topic "quest.*" "quest.dragon"       `shouldBe` True
      matches Topic "quest.*" "quest"              `shouldBe` False
      matches Topic "quest.#" "quest"              `shouldBe` True
      matches Topic "quest.#" "quest.dragon.north" `shouldBe` True
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $
    it "lostKeys finds the keys that go nowhere" $
      lostKeys "quests" trainingKeys (surely guildPost)
        `shouldBe` Right ["news.dragon.north", "dragon"]
