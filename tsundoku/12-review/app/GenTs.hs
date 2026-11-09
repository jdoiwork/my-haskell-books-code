-- ANCHOR: all
{-# LANGUAGE DataKinds #-}
-- 🟦 Write the TypeScript types of the API's JSON, for the screens
--    written in TypeScript (React, in a later chapter).
-- API の JSON の TypeScript の型を書き出す。TypeScript で書く画面
-- （あとの章の React）のために
module Main (main) where

import Data.Aeson.TypeScript.TH
import Data.Proxy (Proxy (..))

import Tsundoku.Api.Json

main :: IO ()
main = do
  putStrLn "// Made by `cabal run -v0 gen-ts` from Tsundoku.Api.Json."
  putStrLn "// Tsundoku.Api.Json から `cabal run -v0 gen-ts` で作ったもの"
  putStrLn ""
  putStrLn (formatTSDeclarations' options declarations)
  where
    options = defaultFormattingOptions { exportMode = ExportEach }
    declarations = concat
      [ getTypeScriptDeclarations (Proxy @WorkSummary)
      , getTypeScriptDeclarations (Proxy @ProgressNumbers)
      , getTypeScriptDeclarations (Proxy @CardDetail)
      , getTypeScriptDeclarations (Proxy @TocItem)
      , getTypeScriptDeclarations (Proxy @NoteItem)
      , getTypeScriptDeclarations (Proxy @ReadingNow)
      , getTypeScriptDeclarations (Proxy @HitItem)
      , getTypeScriptDeclarations (Proxy @Snip)
      , getTypeScriptDeclarations (Proxy @Problem)
      , getTypeScriptDeclarations (Proxy @NewWork)
      , getTypeScriptDeclarations (Proxy @Outline)
      , getTypeScriptDeclarations (Proxy @NewNote)
      , getTypeScriptDeclarations (Proxy @Happened)
      , getTypeScriptDeclarations (Proxy @ReadOn)
      , getTypeScriptDeclarations (Proxy @Points)
      , getTypeScriptDeclarations (Proxy @FormField)
      ]
-- ANCHOR_END: all
