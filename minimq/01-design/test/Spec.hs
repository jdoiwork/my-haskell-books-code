module Main (main) where

import Data.Foldable (toList)
import Test.Hspec
import Test.QuickCheck

import MiniMq.Broker
import MiniMq.Examples
import MiniMq.Types
import MqAnswers

-- 🎲 Letters with one of a few keys, so that some go nowhere.
-- いくつかの札のどれかを付けた手紙（どこにも届かないものも出るように）
genLetter :: Gen Message
genLetter = do
  key  <- elements ["dragon", "herb", "goblin"]
  text <- elements ["a", "b", "c"]
  pure (Message key text)

-- 🏤 The guild's post office, which we know is fine.
-- ギルドの伝令所（ちゃんと作れることがわかっている）
post :: Broker
post = surely guildPost

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "publish" $ do
    -- 札が合う郵便受けへ届く
    it "sends a letter to the mailbox whose key matches" $
      fmap fst (publish "quests" (Message "dragon" "Red") post)
        `shouldBe` Right (Routed (pure "dragon-team"))
    -- どの決まりにも合わない手紙は、Unroutable
    it "says Unroutable when no rule matches" $
      fmap fst (publish "quests" (Message "goblin" "Boo") post)
        `shouldBe` Right Unroutable
    -- ない仕分け台へは送れない
    it "fails for a desk that does not exist" $
      fmap fst (publish "kitchen" (Message "dragon" "Red") post)
        `shouldBe` Left (NoSuchExchange "kitchen")
    -- 手紙は、入れた順に出てくる
    it "keeps letters in order (first in, first out)" $
      forAll (listOf genLetter) $ \letters ->
        let dragons = filter ((== "dragon") . routingKey) letters
            taken = do
              (_, b) <- publishAll "quests" letters post
              fst <$> drain "dragon-team" b
        in  taken === Right dragons
    -- 手紙は、なくならず、増えもしない
    it "neither loses nor copies a letter" $
      forAll (listOf genLetter) $ \letters ->
        case publishAll "quests" letters post of
          Left err -> counterexample (show err) False
          Right (outcomes, b) ->
            sum (map snd (waiting b)) === sum (map delivered outcomes)
  -- ANCHOR_END: spec

  describe "declare" $ do
    -- もう一度作っても、手紙は消えない
    it "keeps the letters when a queue is declared again" $
      let (_, b) = surely (publish "quests" (Message "herb" "Mint") post)
      in  waiting (declareQueue "herb-team" b) `shouldBe` waiting b
    -- ないキューには結べない
    it "cannot bind to a missing queue" $
      bind "quests" "x" "nobody" post `shouldBe` Left (NoSuchQueue "nobody")

  -- 「トレーニング」
  describe "answers" $ do
    it "purge counts and empties" $ do
      let (_, b) = surely (publishAll "quests" trainingLetters post)
      fmap fst (purge "herb-team" b) `shouldBe` Right 2
      fmap (waiting . snd) (purge "herb-team" b)
        `shouldBe` Right [("dragon-team", 1), ("herb-team", 0)]
    it "drain keeps the order" $
      let (_, b) = surely (publishAll "quests" trainingLetters post)
      in  fmap (map body . fst) (drain "herb-team" b)
            `shouldBe` Right ["Mint", "Sage"]
  where
    delivered (Routed qs) = length (toList qs)
    delivered Unroutable  = 0

