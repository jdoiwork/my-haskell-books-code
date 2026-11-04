{-# LANGUAGE QuasiQuotes #-}
-- This file is meant NOT to compile: the closing tag is wrong.
-- このファイルは、わざとコンパイルできない：閉じタグがまちがっている
module WrongClose where

import Hsx

card :: Html
card = [hsx|
  <div class="card">
    <h2>Lambda</h2>
    <p>Level 12</div>
  </div>
|]
