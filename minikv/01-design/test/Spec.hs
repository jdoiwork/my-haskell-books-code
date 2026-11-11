module Main (main) where

-- ANCHOR: imports
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import KvAnswers
import MiniKv.Store
import MiniKv.Types
-- ANCHOR_END: imports

-- 🎲 Random commands on a few keys, so that kinds collide often.
-- ANCHOR: gens
-- 少ないキーへの、でたらめなコマンド（種類がよくぶつかるように）
genCommand :: Gen Command
genCommand = oneof
  [ Get <$> key, Set <$> key <*> text, Del <$> listOf key
  , Exists <$> key, Incr <$> key, IncrBy <$> key <*> small
  , LPush <$> key <*> listOf text, RPush <$> key <*> listOf text
  , LPop <$> key, RPop <$> key, LRange <$> key <*> small <*> small
  , LLen <$> key, SAdd <$> key <*> listOf text
  , SRem <$> key <*> listOf text, SMembers <$> key
  , SIsMember <$> key <*> text, HSet <$> key <*> text <*> text
  , HGet <$> key <*> text, HGetAll <$> key, TypeOf <$> key, pure Keys
  ]
  where
    key   = elements ["a", "b", "c"]
    -- the edge of Int, so that INCR overflows now and then
    -- Int の端の数も。INCR が、ときどきあふれるように
    text  = elements
      ["x", "y", "z", "1", "2", T.pack (show (maxBound :: Int))]
    small = choose (-3, 3)

-- 🧺 Is it an empty list, set or hash?
-- 空のリスト・集合・ハッシュか
emptyCollection :: Value -> Bool
emptyCollection v = case v of
  List l    -> null l
  Members s -> null s
  Hash h    -> null h
  Str _     -> False
-- ANCHOR_END: gens

-- ANCHOR: spec
main :: IO ()
main = hspec $ do
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
    -- Int の端を越える INCR は、Overflow のエラーで、ストアは変わらない
    it "INCR at maxBound is an Overflow error" $ do
      let big             = T.pack (show (maxBound :: Int))
          (_, before)     = applyAll [Set "n" big] emptyStore
          (reply, after)  = apply (Incr "n") before
      reply `shouldBe` Failed (Overflow "n")
      after `shouldBe` before
    -- Int に入らない数や、16進の形は、数ではない
    it "INCR refuses numbers too big for Int, and 0x10" $
      fst (applyAll [ Set "a" "9223372036854775808", Incr "a"
                    , Set "b" "0x10", Incr "b" ] emptyStore)
        `shouldBe` [ Ok, Failed (NotAnInteger "a")
                   , Ok, Failed (NotAnInteger "b") ]
    -- 同じキーを2回渡しても、消えたのは1つ
    it "DEL counts a key given twice once" $
      fst (applyAll [Set "a" "1", Del ["a", "a"]] emptyStore)
        `shouldBe` [Ok, Number 1]
    -- どんなコマンドの列のあとも、空の入れ物は残らない
    it "never keeps an empty list, set or hash" $
      forAll (listOf genCommand) $ \commands ->
        let (_, s) = applyAll commands emptyStore
        in  all (not . emptyCollection) (storedValues s)
    -- エラーを答えたコマンドは、ストアを変えない
    it "a failed command leaves the store as it was" $
      forAll (listOf genCommand) $ \commands ->
        forAll genCommand $ \command ->
          let (_, s)       = applyAll commands emptyStore
              (reply, s')  = apply command s
          in  case reply of
                Failed _ -> s' == s
                _        -> True
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
