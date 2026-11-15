-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
-- ANCHOR: all
-- | The prelude: Lam's standard library, written in Lam itself.
-- プレリュード：Lam で書いた、Lam の標準ライブラリ。
module Lam.Prelude
  ( preludeSource
  , preludeSession
  ) where

import Data.Text (Text)

import Lam.Program
-- ANCHOR_END: head

-- ANCHOR: source
-- 📚 Every REPL and every program starts with these.
-- どの REPL も、どのプログラムも、これを読みこんでから始まる
preludeSource :: Text
preludeSource = """
  -- Functions about functions
  -- 関数についての関数
  let id x = x
  let const x y = x
  let compose f g x = f (g x)
  let flip f x y = f y x
  let twice f x = f (f x)

  -- true and false
  -- 真と偽
  let not b = if b then false else true
  let and a b = if a then b else false
  let or a b = if a then true else b

  -- Numbers
  -- 数
  let max a b = if a < b then b else a
  let min a b = if a < b then a else b
  let abs n = if n < 0 then 0 - n else n
  let mod a b = a - a / b * b
  let even n = mod n 2 == 0
  let pow b e = if e == 0 then 1 else b * pow b (e - 1)
  let gcd a b = if b == 0 then a else gcd b (mod a b)

  -- Lists, built on the builtins null, head and tail
  -- リスト。組み込みの null・head・tail の上に作る
  let foldr f z xs =
    if null xs then z else f (head xs) (foldr f z (tail xs))
  let foldl f z xs =
    if null xs then z else foldl f (f z (head xs)) (tail xs)
  let map f xs = foldr (\\x rest -> f x : rest) [] xs
  let filter p xs =
    foldr (\\x rest -> if p x then x : rest else rest) [] xs
  let sum xs = foldl (\\total x -> total + x) 0 xs
  let reverse xs = foldl (\\acc x -> x : acc) [] xs
  let range a b = if a > b then [] else a : range (a + 1) b
  """
-- ANCHOR_END: source

-- ANCHOR: session
-- 🌱 A session with the prelude loaded. The prelude is checked by
--    the tests, so a mistake in it is found before anyone runs Lam.
-- プレリュードを読みこんだセッション。プレリュードはテストで
-- 確かめるので、まちがいは、誰かが Lam を動かす前に見つかる
preludeSession :: Session
preludeSession =
  case loadProgram emptySession "<prelude>" preludeSource of
    Right (s, _) -> s
    Left err     -> error ("the prelude is broken: " <> show err)
-- ANCHOR_END: session
-- ANCHOR_END: all
