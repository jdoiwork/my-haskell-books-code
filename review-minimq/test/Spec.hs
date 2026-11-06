module Main (main) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Review.Dispatch

-- ANCHOR: gen
-- 🎲 Some orders (about one in five cursed), numbered 1, 2, 3, ...
-- いくつかの依頼（5つに1つくらいが呪われている）。番号は 1, 2, 3, ...
genOrders :: Gen [Order]
genOrders = do
  curses <- listOf (frequency [(4, pure False), (1, pure True)])
  pure (numbered curses)

-- ✂️ Fewer orders, or fewer curses; numbered again from 1.
-- 依頼を減らす、呪いを減らす。番号は 1 から振り直す
shrinkOrders :: [Order] -> [[Order]]
shrinkOrders = map numbered . shrink . map cursed

numbered :: [Bool] -> [Order]
numbered = zipWith Order [1 ..]
-- ANCHOR_END: gen

main :: IO ()
main = hspec $
  describe "a day at the guild" $ do
    -- ANCHOR: calm
    -- 1人も倒れない日：どの依頼もちょうど1回
    it "does every good order once on a calm day" $ do
      let todo = [Order 1 False, Order 2 True, Order 3 False]
      report <- runDay Forgetful 2 todo []
      doneCount report `shouldBe` Map.fromList [(1, 1), (3, 1)]
      buried report `shouldBe` [2]
    -- ANCHOR_END: calm
    -- ANCHOR: lossless
    -- どんなに倒れても：良い依頼は1回以上こなされ、呪いの依頼は墓場へ
    it "loses nothing, however often workers fall" $
      withMaxSuccess 30 $
        forAll genOrders $ \todo -> forAll (listOf arbitrary) $ \falls ->
          monadicIO $ do
            got <- run (timeout 10000000 (runDay Forgetful 3 todo falls))
            let good  = [orderNo o | o <- todo, not (cursed o)]
                bad   = [orderNo o | o <- todo, cursed o]
            case got of
              Nothing -> assert False
              Just report -> do
                let done = doneCount report
                -- どの良い依頼も、1回以上こなされた
                assert (all (`Map.member` done) good)
                -- 呪いの依頼は、みんな墓場に
                assert (all (`elem` buried report) bad)
                -- 墓場にいる良い依頼は、じつは終わっていた
                assert (all (\n -> n `elem` bad || Map.member n done)
                            (buried report))
    -- ANCHOR_END: lossless
    -- ANCHOR: draft
    -- 下書きの性質「墓場には呪いの依頼だけ」は、まちがっている
    it "draft: only cursed orders are buried (QuickCheck breaks it)" $
      expectFailure $ withMaxSuccess 200 $
        forAllShrink genOrders shrinkOrders $ \todo ->
        forAllShrink (listOf arbitrary) shrink $ \falls ->
          monadicIO $ do
            got <- run (timeout 10000000 (runDay Forgetful 1 todo falls))
            let bad = [orderNo o | o <- todo, cursed o]
            assert (fmap (sort . buried) got == Just bad)
    -- ANCHOR_END: draft
    -- ANCHOR: remember
    -- 覚えている働き手なら、どの依頼も、こなすのは1回だけ
    it "a remembering worker does each order only once" $
      withMaxSuccess 30 $
        forAll genOrders $ \todo -> forAll (listOf arbitrary) $ \falls ->
          monadicIO $ do
            got <- run (timeout 10000000 (runDay Remembering 3 todo falls))
            assert (fmap (all (== 1) . doneCount) got == Just True)
    -- ANCHOR_END: remember
