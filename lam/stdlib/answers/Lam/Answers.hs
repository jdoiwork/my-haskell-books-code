-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "Fill the toolbox: the standard
--   library of strings, lists and maps".
-- 「道具箱をそろえる：文字列・リスト・Map の標準ライブラリ」の
-- トレーニングの解答。
module Lam.Answers
  ( indexOf
  , withBuiltins
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T

import Lam.Builtins
import Lam.Eval
import Lam.Program (Session (..))
import Lam.Types

-- ANCHOR: indexOf
-- 🔎 Step 4: where a text first comes in a string, counted in letters
--    from 0, or Nothing when it is not there (Python's str.find gives
--    -1 instead).
--      String.indexOf "am" "Lam and Haskell"  is  Just 1
-- ステップ4：文字列の中で、ある文字列が最初に出てくる場所（0 から
-- 数えた文字の数）。なければ Nothing（Python の str.find は -1）
indexOf :: Builtin
indexOf =
  prim2 "String.indexOf" (tString ~> tString ~> TCon "Maybe" [TInt]) $
    \x v -> do
      needle <- string x
      t      <- string v
      -- An empty text is at the very start (T.breakOn would stop the
      -- program on it, so it is not asked).
      -- 空の文字列は、いちばん頭にある（T.breakOn は空の文字列で
      -- プログラムを止めるので、聞かない）
      let (before, after) = T.breakOn needle t
      Right $ maybeValue $
        if T.null needle then Just (VInt 0)
        else if T.null after then Nothing
        else Just (VInt (fromIntegral (T.length before)))
-- ANCHOR_END: indexOf

-- ANCHOR: withBuiltins
-- 🧰 A session with more builtins: one row each, as in the table.
-- 組み込みを足したセッション。表と同じく、1つ1行
withBuiltins :: [Builtin] -> Session -> Session
withBuiltins more session = session
  { sessionTypes  =
      Map.fromList [ (builtinName b, builtinType b) | b <- more ]
        `Map.union` sessionTypes session
  , sessionValues =
      Map.fromList [ (builtinName b, builtinValue b) | b <- more ]
        `Map.union` sessionValues session }
-- ANCHOR_END: withBuiltins
-- ANCHOR_END: all
