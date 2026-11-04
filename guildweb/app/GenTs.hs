-- | Write the TypeScript types for the React app. "export" is added,
--   so that the other files can import them.
-- React のアプリのための、TypeScript の型を書き出す。ほかのファイルから
-- import できるように、export を付ける
module Main (main) where

import Data.Aeson.TypeScript.TH (formatTSDeclarations)
import Data.List (isPrefixOf)

import Board.Types (tsDeclarations)

main :: IO ()
main =
  writeFile "frontend/src/types.ts" $ unlines $
    [ "// Made by `cabal run gen-ts` from the Haskell types. Do not edit."
    , "// Haskell の型から cabal run gen-ts で作った。手で直さない"
    ]
      <> map exported (lines (formatTSDeclarations tsDeclarations))
  where
    exported line
      | any (`isPrefixOf` line) ["type ", "interface "] = "export " <> line
      | otherwise                                       = line
