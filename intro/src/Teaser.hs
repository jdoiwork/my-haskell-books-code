module Teaser where

-- ANCHOR: squares
-- ⭐️ Square the even numbers from 1 to 10.
-- 1 から 10 までの偶数を2乗する
evenSquares :: [Int]
evenSquares = [ x * x | x <- [1 .. 10], even x ]
-- ANCHOR_END: squares

-- ANCHOR: shape
-- 🔷 A shape is either a circle or a rectangle.
-- 図形は「円」か「長方形」のどちらか
data Shape
  = Circle    Double
  | Rectangle Double Double

-- 📐 Compute the area of a shape.
-- 図形の面積を求める
area :: Shape -> Double
area (Circle    r  ) = pi * r * r
area (Rectangle w h) = w  * h
-- ANCHOR_END: shape
