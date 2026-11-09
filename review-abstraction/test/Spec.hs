module Main (main) where

import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import Review.Ledger

sampleLines :: [String]
sampleLines =
  [ "join Lambda", "reward Lambda 50", "join Python"
  , "reward Python 30", "fine Lambda 80" ]

-- ANCHOR: events
-- 🎲 Random events among three adventurers.
-- 3人の冒険者の、でたらめな出来事
newtype Events = Events [Event]
  deriving (Show)

instance Arbitrary Events where
  arbitrary = Events <$> listOf event
    where
      who = elements ["Lambda", "Python", "Rust"]
      amount = chooseInt (0, 100)
      event = oneof
        [ Join <$> who, Reward <$> who <*> amount, Fine <$> who <*> amount ]
-- ANCHOR_END: events

main :: IO ()
main = hspec $ do
  describe "parseAll" $ do
    -- 全部よければ、出来事のリスト
    it "reads good lines" $
      parseAll ["join Lambda", "reward Lambda 50"] `shouldBe`
        Valid [Join "Lambda", Reward "Lambda" 50]
    -- 悪い行は、全部まとめて知らせる
    it "reports every bad line" $
      parseAll ["join Lambda", "reward Lambda lots", "dance", "fine Rust -3"]
        `shouldBe` Invalid
          [ "line 2: bad amount lots", "line 3: unknown event"
          , "line 4: bad amount -3" ]
  describe "ledger" $ do
    -- 罰金で 0 まで下がると、non 0 が帳簿から名前を消す
    it "applies the sample" $ do
      let Valid events = parseAll sampleLines
          ledger = runEvents events emptyLedger
      ledger ^. gold `shouldBe` Map.fromList [("Python", 30)]
      summarize ledger `shouldBe` Summary 30 30 1
    -- ANCHOR: split
    -- 前半と後半に分けて当てはめても、一度に当てはめても、同じ帳簿
    it "splitting the events does not matter" $ property $
      \(Events xs) (Events ys) ->
        runEvents (xs ++ ys) emptyLedger
          == runEvents ys (runEvents xs emptyLedger)
    -- ANCHOR_END: split
    -- ゴールドは、決して負にならない
    it "gold never goes negative" $ property $ \(Events xs) ->
      all (>= 0) (Map.elems (runEvents xs emptyLedger ^. gold))
