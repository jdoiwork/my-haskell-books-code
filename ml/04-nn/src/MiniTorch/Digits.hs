-- ANCHOR: all
-- | Hand-written-looking digits: 5 x 7 pixel glyphs with noise.
--   (A small stand-in for MNIST, made by the program itself.)
-- 手書き風の数字：5 × 7 の点の字に、ノイズを足したもの。
-- （MNIST の代わりに、プログラムで作る小さなデータ）
module MiniTorch.Digits
  ( glyphs
  , makeDigits
  , showDigit
  ) where

import Data.List (unfoldr)
import Data.Word (Word64)

import qualified MiniNumpy as Np
import MiniNumpy (NDArray)
import MiniTorch.Random (uniforms)

-- ANCHOR: glyphs
-- 🔢 The ten digits, 7 rows of 5 pixels each.
-- 10個の数字。5つの点が7行
glyphs :: [[String]]
glyphs =
  [ ["01110","10001","10011","10101","11001","10001","01110"]  -- 0
  , ["00100","01100","00100","00100","00100","00100","01110"]  -- 1
  , ["01110","10001","00001","00010","00100","01000","11111"]  -- 2
  , ["11111","00010","00100","00010","00001","10001","01110"]  -- 3
  , ["00010","00110","01010","10010","11111","00010","00010"]  -- 4
  , ["11111","10000","11110","00001","00001","10001","01110"]  -- 5
  , ["00110","01000","10000","11110","10001","10001","01110"]  -- 6
  , ["11111","00001","00010","00100","01000","01000","01000"]  -- 7
  , ["01110","10001","10001","01110","10001","10001","01110"]  -- 8
  , ["01110","10001","10001","01111","00001","00010","01100"]  -- 9
  ]
-- ANCHOR_END: glyphs

-- ANCHOR: make
-- 🏭 n digits (0, 1, ..., 9, 0, 1, ...), each pixel flipped with
--    probability `noise`. Returns X [n, 35] and the labels [n].
-- n 個の数字。それぞれの点は、確率 noise で白黒が入れかわる
makeDigits :: Word64 -> Double -> Int -> (NDArray, [Int])
makeDigits seed noise n =
  let labels = [ i `mod` 10 | i <- [0 .. n - 1] ]
      clean d = [ if c == '1' then 1 else 0 | row <- glyphs !! d, c <- row ]
      chunks  = unfoldr (\us -> Just (splitAt 35 us)) (uniforms seed)
      flipIf p u = if u < noise then 1 - p else p
      noisy d us = zipWith flipIf (clean d) us
      pixels  = concat (zipWith noisy labels chunks)
  in (Np.reshape [n, 35] (Np.array pixels), labels)
-- ANCHOR_END: make

-- ANCHOR: show
-- 🖼️ Draw one digit (35 numbers) with # and .
-- 1つの数字（35個の数）を、# と . で描く
showDigit :: [Double] -> String
showDigit px = unlines [ [ if p > 0.5 then '#' else '.' | p <- row ]
                       | row <- rowsOf px ]
  where
    rowsOf [] = []
    rowsOf xs = let (r, rest) = splitAt 5 xs in r : rowsOf rest
-- ANCHOR_END: show
-- ANCHOR_END: all
