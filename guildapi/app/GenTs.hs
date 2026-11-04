-- ANCHOR: all
-- 🟦 Print TypeScript types for the API's data, for the React side.
--    "export" is added, so that other files can import them.
-- API のデータの TypeScript の型を、React 側のために書き出す。
-- ほかのファイルから import できるように、export を付ける
module Main (main) where

import Data.List (isPrefixOf)

import Data.Aeson.TypeScript.TH
import Data.Proxy (Proxy (..))

import Guild.Types

main :: IO ()
main = putStr (unlines (map exported (lines declarations)))
  where
    declarations = formatTSDeclarations $ concat
      [ getTypeScriptDeclarations (Proxy :: Proxy Adventurer)
      , getTypeScriptDeclarations (Proxy :: Proxy NewAdventurer)
      , getTypeScriptDeclarations (Proxy :: Proxy ApiError)
      ]
    exported line
      | any (`isPrefixOf` line) ["type ", "interface "] = "export " ++ line
      | otherwise                                         = line
-- ANCHOR_END: all
