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
