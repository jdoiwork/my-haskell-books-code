{-# LANGUAGE OverloadedStrings #-}
-- | Print the roster page. Try:  cabal run guild-page > roster.html
-- 名簿のページを表示する。cabal run guild-page > roster.html で試せる。
module Main (main) where

import qualified Data.Text.IO as T

import GuildPage
import Hsx (renderHtml)

main :: IO ()
main = T.putStrLn (renderHtml (rosterPage "Lambda Guild" members))
  where
    members =
      [ Adventurer "Lambda"  "Wizard"  12 (Just "Archmage")
      , Adventurer "Python"  "Ranger"   7 Nothing
      -- A name that tries to inject a script is shown as text.
      -- スクリプトを仕込もうとした名前は、文字として表示される
      , Adventurer "<script>alert(1)</script>" "Thief" 1 Nothing
      ]
