-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Answers to the training of the chapter on Map and JSON.
-- Map と JSON の章のトレーニングの解答。
module Lam.Answers
  ( prettyBuiltin
  , withBuiltin
  ) where

import Data.Aeson.Encode.Pretty
  (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)

import Lam.Builtins
import Lam.Eval (Value (..))
import Lam.Json (toAeson)
import Lam.Program
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: pretty
-- 🎀 Step 4: Json.pretty writes JSON over many lines, with 2 spaces
--    of indent. aeson-pretty does the work; this is one more row
--    for the table of builtins.
-- ステップ4：Json.pretty は、JSON を2つの空白で字下げして、何行にも
-- 分けて書く。仕事は aeson-pretty がする。組み込みの関数の表の1行
prettyBuiltin :: Builtin
prettyBuiltin =
  prim1 "Json.pretty" (TCon "Json" [] ~> tString) $ \v -> do
    j <- toAeson v
    let config = defConfig { confIndent = Spaces 2 }
    Right (VString
      (TL.toStrict (toLazyText (encodePrettyToTextBuilder' config j))))

-- 🧰 A session with one more builtin (its type and its value).
-- 組み込みの関数を1つ足したセッション（型と値）
withBuiltin :: Builtin -> Session -> Session
withBuiltin b s = s
  { sessionTypes  = Map.insert (builtinName b) (builtinType b)
                      (sessionTypes s)
  , sessionValues = Map.insert (builtinName b) (builtinValue b)
                      (sessionValues s) }
-- ANCHOR_END: pretty
-- ANCHOR_END: all
