-- ANCHOR: all
-- 📊 How many times each damage (0 to 96) came up: a mutable vector.
-- ダメージ（0〜96）ごとの回数：書き換えられる Vector で
module Histogram (histogram) where

import Control.Monad.ST            (runST)
import Data.Vector.Unboxed         qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

-- ANCHOR: histogram
-- 📊 Count the damages into 97 boxes, rewriting them in place.
-- ダメージを、97個の箱に、その場で書き換えながら数える
histogram :: [Int] -> VU.Vector Int
histogram damages = runST $ do
  counts <- MVU.replicate 97 0
  mapM_ (\d -> MVU.modify counts (+ 1) d) damages
  VU.freeze counts
-- ANCHOR_END: histogram
-- ANCHOR_END: all
