{-# LANGUAGE QuasiQuotes #-}
-- This file is meant NOT to compile: {nmae} is a typo of {name}.
-- このファイルは、わざとコンパイルできない：{nmae} は {name} の打ちまちがい
module Typo where

import Data.Text (Text)

import Hsx

greeting :: Text -> Html
greeting name = [hsx|<p>Hello, {nmae}!</p>|]
