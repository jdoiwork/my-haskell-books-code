{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | The answers of the training of "Making type classes: class and
--   instance".
-- 「型クラスを作る：class と instance」のトレーニングの解答。
module Lam.Answers
  ( instanceList
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Program (Session (..))
import Lam.Types (DataTable (..))

-- ANCHOR: instanceList
-- 📋 Step 4: the instances there are now, one a line, in the order of
--    the class and the name of the type (the keys of the table).
--    instance Describe Shape
-- ステップ4：いまあるインスタンスを1行ずつ。クラスと型の名前の順
-- （表のキーの順）
instanceList :: Session -> Text
instanceList session = T.intercalate "\n"
  [ "instance " <> c <> " " <> t
  | (c, t) <- Map.keys (instances (sessionData session)) ]
-- ANCHOR_END: instanceList
-- ANCHOR_END: all
