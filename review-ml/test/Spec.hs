-- ANCHOR: head
module Main (main) where

import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.QuickCheck

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.NN (Params)
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
        total   = sum (map Np.size (Map.elems ps))
    in  counterexample (show ds)
          (length ds == total && all (\d -> abs (d - lr) < 1e-6) ds)
-- ANCHOR_END: firstStep

main :: IO ()
main = hspec $ do
  describe "optimizers" $ do
    -- momentum 0 の Momentum は、ふつうの SGD と同じ
    it "momentum with mu = 0 is plain SGD" $
      forAll genCase $ \(ps, grads) ->
        fst (step (momentum 0.1 0) ps grads)
          == fst (step (sgd 0.1) ps grads)
    -- Momentum の2歩目は、1歩目の速度を覚えている
    it "momentum remembers the last velocity" $ do
      let ps    = Map.fromList [("w", Np.array [0])]
          grads = Map.fromList [("w", Np.array [1])]
          (ps1, opt1) = step (momentum 0.1 0.9) ps grads
          (ps2, _)    = step opt1 ps1 grads
      -- 1歩目は 0.1、2歩目は 0.1 × (0.9 + 1) = 0.19 動く
      let w = concatMap Np.toList (Map.elems ps2)
      map (\v -> abs (v - (-0.29)) < 1e-12) w `shouldBe` [True]
    -- 傾きが 0 なら、Adam は動かない
    it "Adam does not move when the gradients are 0" $
      forAll genCase $ \(ps, grads) ->
        let zero = Map.map (Np.map (const 0)) grads
        in  fst (step (adam defaultAdam) ps zero) == ps
    -- ANCHOR: draftTest
    -- Adam の最初の1歩は、どの数も lr だけ動く
    it "Adam's first step moves every number by lr" $
      prop_adamFirstStep adam
    -- 下書きは、この性質を破る
    it "the draft breaks it" $
      expectFailure (prop_adamFirstStep adamDraft)
    -- ANCHOR_END: draftTest
