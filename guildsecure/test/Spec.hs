-- ANCHOR: all
-- ANCHOR: imports
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- 🧪 Tests for the security chapter.
-- セキュリティの章のテスト
module Main (main) where

import Data.Aeson               (Value, decode, eitherDecode)
import Data.ByteString.Lazy     qualified as BL
import Data.Text                (Text)
import Data.Text                qualified as T
import Database.SQLite.Simple
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Secure.Answers
import Secure.Authz
import Secure.Holey             (canDeleteDraft, questsByOwnerUnsafe)
import Secure.Limits
import Secure.Secret
import Secure.Username
-- ANCHOR_END: imports

-- ANCHOR: arbitrary
-- 🎲 Random members and quests, with only a few names,
--    so that "the same name" happens often.
-- でたらめな一員と依頼。名前を少なくして、「同じ名前」がよく起きるようにする
names :: Gen Text
names = elements ["lambda", "python", "rust"]

instance Arbitrary Role where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Member where
  arbitrary = Member <$> names <*> arbitrary

instance Arbitrary Quest where
  arbitrary = Quest <$> elements ["slay the slime", "find the herb"] <*> names
-- ANCHOR_END: arbitrary

main :: IO ()
main = hspec $ do
  describe "parseUsername" $ do
    -- 「ふつうの名前は通す」
    it "accepts a plain name" $
      fmap usernameText (parseUsername "lambda_3") `shouldBe` Right "lambda_3"
    -- 「SQL のかけらは断る」
    it "turns away a piece of SQL" $
      parseUsername "x' OR '1'='1" `shouldBe` Left (BadChar '\'')
    -- 「長すぎる名前は断る」
    it "turns away a long name" $
      parseUsername (T.replicate 100 "a") `shouldBe` Left TooLong

  describe "Login" $
    -- 「show してもパスワードは出ない」
    it "never shows the password" $ do
      let body       = "{\"user\": \"lambda\", \"password\": \"hunter2\"}"
          Just login = decode body
      show (login :: Login) `shouldNotContain` "hunter2"

  -- ANCHOR: authz
  describe "canEdit" $ do
    -- 「書き換えられるのは、マスターか、依頼主だけ」
    prop "lets only the master or the owner edit" $
      \m q -> canEdit m q ==> m.role == Master || m.name == q.owner
    -- 「マスターと、冒険者の依頼主は、本当に書き換えられる」
    prop "lets the master and the owner edit" $
      \m q -> m.role == Master || (m.role == Adventurer && m.name == q.owner)
               ==> canEdit m q
    -- 「下書きは、この決まりを破る（QuickCheck が見つける）」
    prop "the draft breaks the rule (QuickCheck finds it)" $
      expectFailure $
        \m q -> canEditDraft m q ==> m.role == Master || m.name == q.owner
  -- ANCHOR_END: authz

  describe "limits" $ do
    -- 「大きすぎる本体は、読む前に断る」
    it "refuses a body that is too large" $
      (decodeLimited 10 "[1,2,3,4,5,6,7,8,9]" :: Either String Value)
        `shouldBe` Left "the body is too large"
    -- 「終わらない本体でも、上限のところで止まって断る」
    it "refuses an endless body without reading it all" $
      (decodeLimited 8192 (BL.cycle "[1,") :: Either String Value)
        `shouldBe` Left "the body is too large"
    -- 「1e1000000000 は金額にならない」
    it "does not turn 1e1000000000 into gold" $
      (goldFromJson <$> decode "1e1000000000") `shouldBe` Just Nothing
    -- 「aeson は巨大な指数の Integer を断る」
    it "aeson refuses an Integer with a huge exponent" $
      (eitherDecode "1e1000000000" :: Either String Integer)
        `shouldSatisfy` either (const True) (const False)

  -- answers for the training / トレーニングの解答
  describe "answers" $ do
    -- 「ステップ1：トークンを show しても出ない」
    it "step 1: never shows the token" $
      show (mkApiToken "secret-123") `shouldNotContain` "secret-123"
    -- 「ステップ2：穴あきの版は全部の依頼をもらし、直した版はもらさない」
    it "step 2: the fixed version does not leak every quest" $
      withConnection ":memory:" $ \conn -> do
        execute_ conn "CREATE TABLE quests (title TEXT, owner TEXT)"
        execute_ conn
          "INSERT INTO quests VALUES ('a', 'lambda'), ('b', 'rust')"
        leaked <- questsByOwnerUnsafe conn "x' OR '1'='1"
        length leaked `shouldBe` 2
        safe <- questsByOwner conn "x' OR '1'='1"
        safe `shouldBe` []
    -- ANCHOR: deleteprop
    -- 「ステップ3：消せるのはマスターだけ」
    prop "step 3: only the master may delete" $
      \m q -> canDelete m q ==> m.role == Master
    -- 「ステップ3：下書きはこの決まりを破る」
    prop "step 3: the draft breaks the rule" $
      expectFailure $ \m q -> canDeleteDraft m q ==> m.role == Master
    -- ANCHOR_END: deleteprop
-- ANCHOR_END: all
