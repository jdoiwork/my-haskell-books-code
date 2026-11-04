module Main (main) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import KvAnswers
import MiniKv.Store
import MiniKv.Types

-- 🎲 Random commands on a few keys, so that kinds collide often.
-- 少ないキーへの、でたらめなコマンド（種類がよくぶつかるように）
genCommand :: Gen Command
genCommand = oneof
  [ Get <$> key, Set <$> key <*> text, Del <$> listOf key
  , Incr <$> key, LPush <$> key <*> listOf text, RPop <$> key
  , LPop <$> key, SAdd <$> key <*> listOf text, SRem <$> key <*> listOf text
  , HSet <$> key <*> text <*> text, LRange <$> key <*> small <*> small
  ]
  where
    key   = elements ["a", "b", "c"]
    text  = elements ["x", "y", "z", "1", "2"]
    small = choose (-3, 3)

main :: IO ()
main = hspec $ do
  -- ANCHOR: spec
  describe "apply" $ do
    -- SET したものは、GET で返ってくる
    it "GET returns what SET stored" $
      property $ \k v ->
        let key = T.pack k
            val = T.pack v
            (_, s) = apply (Set key val) emptyStore
        in  fst (apply (Get key) s) == Bulk val
    -- LPUSH は先頭に積むので、逆順に並ぶ
    it "LPUSH puts items at the front" $
      fst (applyAll [LPush "l" ["a", "b", "c"], LRange "l" 0 (-1)] emptyStore)
        `shouldBe` [Number 3, Many ["c", "b", "a"]]
    -- 種類のちがう値に使うと、WrongType のエラー
    it "a list command on a string is a WrongType error" $
      fst (applyAll [Set "k" "v", LPush "k" ["x"]] emptyStore)
        `shouldBe` [Ok, Failed (WrongType "k" KList KString)]
    -- Int の端を越える INCR は、Overflow のエラー
    it "INCR at maxBound is an Overflow error" $
      fst (applyAll [Set "n" (T.pack (show (maxBound :: Int))), Incr "n"]
                    emptyStore)
        `shouldBe` [Ok, Failed (Overflow "n")]
    -- どんなコマンドの列のあとも、空の入れ物は残らない
    it "never keeps an empty list, set or hash" $
      forAll (listOf genCommand) $ \commands ->
        let (_, s) = applyAll commands emptyStore
        in  all (not . emptyCollection) (storedValues s)
  -- ANCHOR_END: spec

  -- 「トレーニング」
  describe "answers" $ do
    it "getSet returns the old value" $
      fst (getSet "k" "new" (snd (apply (Set "k" "old") emptyStore)))
        `shouldBe` Bulk "old"
    it "kindCounts counts each kind" $
      kindCounts (snd (applyAll [Set "a" "1", Set "b" "2", SAdd "s" ["x"]]
                                emptyStore))
        `shouldBe` [(KString, 2), (KSet, 1)]
  where
    emptyCollection v = case v of
      List l    -> null l
      Members s -> null s
      Hash h    -> null h
      Str _     -> False
