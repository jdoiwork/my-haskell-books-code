module Main (main) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
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

-- ANCHOR: model
-- 📜 The rules of the guild's post office, written down as plain pairs.
-- ギルドの伝令所の決まりを、ただの組のリストで書き出したもの
guildRules :: [(RoutingKey, QueueName)]
guildRules = [("dragon", "dragon-team"), ("herb", "herb-team")]

-- 🐉 A busier office: dragon letters also go to the herb team
--    (to heal the wounded), and the herb rule is written twice.
-- にぎやかな伝令所：ドラゴンの手紙は薬草班にも届き（けが人の手当て）、
-- 薬草の決まりは2回書いてある
busyPost :: Broker
busyPost = surely (bind "quests" "dragon" "herb-team" post
                     >>= bind "quests" "herb" "herb-team")

busyRules :: [(RoutingKey, QueueName)]
busyRules = guildRules ++ [("dragon", "herb-team"), ("herb", "herb-team")]

-- 📋 Every mailbox of the office.
-- 伝令所の郵便受け、全部
mailboxes :: [QueueName]
mailboxes = ["dragon-team", "herb-team"]

-- 🧮 The mailboxes a letter should reach, worked out by hand
--    from the rules (no broker here).
-- 手紙が届くべき郵便受けを、決まりから手で求める（ブローカーは使わない）
shouldReach :: [(RoutingKey, QueueName)] -> Message -> [QueueName]
shouldReach rules letter =
  [ queue | queue <- mailboxes, (routingKey letter, queue) `elem` rules ]

-- 🧮 The letters a mailbox should hold, in the order they were sent.
-- 郵便受けに入っているべき手紙を、送った順に
shouldHold :: [(RoutingKey, QueueName)] -> QueueName -> [Message]
           -> [Message]
shouldHold rules queue letters =
  [ letter | letter <- letters, queue `elem` shouldReach rules letter ]

-- 📤 Send the letters with the real broker, then empty one mailbox.
-- 本物のブローカーで手紙を送り、郵便受けを1つ空にする
mailbox :: Broker -> QueueName -> [Message]
        -> Either BrokerError [Message]
mailbox broker queue letters = do
  (_, broker') <- publishAll "quests" letters broker
  fst <$> drain queue broker'

-- 📮 The mailboxes an outcome names, in order.
-- 結果が答えた郵便受け（並べて）
reached :: Outcome -> [QueueName]
reached (Routed qs) = sort (toList qs)
reached Unroutable  = []
-- ANCHOR_END: model

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
    -- 1通の手紙が、2つの郵便受けに1通ずつ届く
    it "sends one letter to two mailboxes, once each" $
      fmap fst (publish "quests" (Message "dragon" "Red") busyPost)
        `shouldBe` Right (Routed ("dragon-team" :| ["herb-team"]))
    -- どの郵便受けにも、届くべき手紙が、送った順に入っている
    it "fills every mailbox just as the rules say, in order" $
      forAll (listOf genLetter) $ \letters ->
        conjoin
          [ mailbox broker queue letters
              === Right (shouldHold rules queue letters)
          | (broker, rules) <- [(post, guildRules), (busyPost, busyRules)]
          , queue <- mailboxes ]
    -- 答えの Outcome も、決まりから求めた行き先と同じ
    it "answers with the mailboxes the rules say" $
      forAll (listOf genLetter) $ \letters ->
        conjoin
          [ fmap (map reached . fst) (publishAll "quests" letters broker)
              === Right (map (shouldReach rules) letters)
          | (broker, rules) <- [(post, guildRules), (busyPost, busyRules)] ]
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

