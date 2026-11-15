-- ANCHOR: all
-- | Small made-up datasets. They use sin, not random numbers,
--   so that Python makes exactly the same numbers.
-- 小さな作りもののデータ。乱数ではなく sin を使うので、
-- Python でも、まったく同じ数ができる。
module MiniSklearn.Datasets
  ( potionPrices
  , monsters
  ) where

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)

-- ANCHOR: potions
-- 🧪 Potion prices: the bigger the potion (ml), the higher the price.
--    price = 3 * size + 5, plus a little wobble.
-- 薬の値段：大きい薬（ml）ほど高い。値段 = 3 × 大きさ + 5 に、少しのゆらぎ
potionPrices :: (NDArray, NDArray)
potionPrices =
  let sizes  = [ fromIntegral i / 5 | i <- [0 .. 49 :: Int] ]
      wobble = [ sin (1.7 * fromIntegral i) | i <- [0 .. 49 :: Int] ]
      prices = zipWith (\s w -> 3 * s + 5 + w) sizes wobble
  in (Np.reshape [50, 1] (Np.array sizes), Np.array prices)
-- ANCHOR_END: potions

-- ANCHOR: monsters
-- 👾 Monsters: attack and speed (0 to 9), and whether they are
--    dangerous (1) or not (0). Strong and fast ones are dangerous.
-- モンスター：攻撃力と素早さ（0〜9）と、危険（1）か、そうでない（0）か
monsters :: (NDArray, NDArray)
monsters =
  let points  = [ (fromIntegral (i `mod` 10), fromIntegral (i `div` 10), i)
                | i <- [0 .. 99 :: Int] ]
      danger (a, s, i) =
        if a + s + 0.8 * sin (2.3 * fromIntegral i) > 9 then 1 else 0
      xs = concat [ [a, s] | (a, s, _) <- points ]
  in (Np.reshape [100, 2] (Np.array xs), Np.array (map danger points))
-- ANCHOR_END: monsters
-- ANCHOR_END: all
