-- ANCHOR: head
module Main (main) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Digits (makeDigits)
import MiniTorch.NN (Params, initParams)
import Review.AdamDraft
import Review.Optim

-- 🎲 A few parameters and gradients of the same shapes.
-- 同じ形のパラメーターと傾きを、いくつか
genCase :: Gen (Params, Params)
genCase = do
  sizes <- listOf1 (choose (1, 5))
  pairs <- mapM genPair (zip [0 :: Int ..] sizes)
  pure (Map.fromList (map fst pairs), Map.fromList (map snd pairs))
  where
    genPair (i, n) = do
      ps <- vectorOf n (choose (-2, 2))
      gs <- vectorOf n (oneof [choose (-3, -0.01), choose (0.01, 3)])
      let name = "p" <> show i
      pure ((name, Np.array ps), (name, Np.array gs))

-- 👣 How far every number moved.
-- それぞれの数が、どれだけ動いたか
moves :: Params -> Params -> [Double]
moves old new =
  concat (Map.elems (Map.intersectionWith distance old new))
  where
    distance :: NDArray -> NDArray -> [Double]
    distance a b = map abs (zipWith (-) (Np.toList a) (Np.toList b))
-- ANCHOR_END: head

-- ANCHOR: firstStep
-- 🦉 Adam's first step moves every number by (almost) exactly lr.
-- Adam の最初の1歩は、どの数も（ほぼ）ちょうど lr だけ動かす
prop_adamFirstStep :: (AdamConfig -> Optimizer) -> Property
prop_adamFirstStep make =
  forAll genCase $ \(ps, grads) ->
    let lr      = 0.01
        opt     = make defaultAdam { learningRate = lr }
        (ps', _) = step opt ps grads
        ds      = moves ps ps'
        -- Every number must be there: all on an empty list is True.
        -- 数が1つも欠けていないこと（空のリストの all は True なので）
        count   = sum (map Np.size (Map.elems ps))
    in  counterexample (show ds)
          (length ds == count && all (\d -> abs (d - lr) < 1e-6) ds)
-- ANCHOR_END: firstStep

-- ANCHOR: main
main :: IO ()
main = hspec $ do
  describe "optimizers" $ do
-- ANCHOR_END: main
    -- ANCHOR: others
    -- momentum 0 の Momentum は、ふつうの SGD と同じ
    it "momentum with mu = 0 is plain SGD" $
      forAll genCase $ \(ps, grads) ->
        fst (step (momentum 0.1 0) ps grads)
          == fst (step (sgd 0.1) ps grads)
    -- Momentum の2歩目は、1歩目の速度を覚えている
    it "momentum remembers the last velocity" $ do
      let ps    = Map.fromList [("w", Np.array [0])]
          g1    = Map.fromList [("w", Np.array [1])]
          g2    = Map.fromList [("w", Np.array [2])]
          (ps1, opt1) = step (momentum 0.1 0.9) ps g1
          (ps2, _)    = step opt1 ps1 g2
      -- 1歩目は 0.1、2歩目は 0.1 × (0.9 × 1 + 2) = 0.29 動く
      let w = concatMap Np.toList (Map.elems ps2)
      map (\v -> abs (v - (-0.39)) < 1e-12) w `shouldBe` [True]
    -- 傾きのない歩は動かず、速度や平均は取っておく（PyTorch と同じ数）
    it "a parameter with no gradient keeps its state" $ do
      let ps   = Map.fromList [("w", Np.array [0])]
          g1   = Map.fromList [("w", Np.array [1])]
          rest = Map.empty
          run opt = let (a, o1) = step opt ps g1
                        (b, o2) = step o1 a rest
                        (c, _)  = step o2 b g1
                    in  concatMap Np.toList (Map.elems c)
      map (\v -> abs (v - (-0.29)) < 1e-12) (run (momentum 0.1 0.9))
        `shouldBe` [True]
      map (\v -> abs (v - (-0.0199999998)) < 1e-9)
          (run (adam defaultAdam { learningRate = 0.01 }))
        `shouldBe` [True]
    -- 25 歩の損失は、PyTorch と小数第4位まで同じ（向きも、覚えることも見る）
    it "trainWith gives PyTorch's losses after 25 steps" $ do
      let (x, ys) = makeDigits 1 0.1 1000
          start   = initParams 7 [("fc1", 35, 32), ("fc2", 32, 10)]
          lossAfter o = last (fst (trainWith o 25 start x ys))
          close a b = abs (a - b) < 5e-5
      let adam01 = adam defaultAdam { learningRate = 0.01 }
      map lossAfter [sgd 0.5, momentum 0.1 0.9, adam01]
        `shouldSatisfy` (and . zipWith close [0.4284, 0.3744, 0.1371])
    -- 作りたての Adam は、傾きが 0 なら動かない
    it "a fresh Adam does not move when the gradients are 0" $
      forAll genCase $ \(ps, grads) ->
        let zero = Map.map (Np.map (const 0)) grads
        in  fst (step (adam defaultAdam) ps zero) == ps
    -- ANCHOR_END: others
    -- ANCHOR: draftTest
    -- Adam の最初の1歩は、どの数も lr だけ動く
    it "Adam's first step moves every number by lr" $
      prop_adamFirstStep adam
    -- 下書きは、この性質を破る
    it "the draft breaks it" $
      expectFailure (prop_adamFirstStep adamDraft)
    -- ANCHOR_END: draftTest
