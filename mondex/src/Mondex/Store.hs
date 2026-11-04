-- ANCHOR: all
-- ANCHOR: module
-- 💾 Reading and writing the bestiary file (IO).
-- 図鑑のファイルを読み書きする（IO）
module Mondex.Store
  ( loadBestiary
  , saveBestiary
  ) where

-- JSON files.
-- JSON のファイル
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL

-- Files, and stopping with an error.
-- ファイルと、エラーで止まる
import System.Directory (doesFileExist)
import System.Exit      (die)

import Mondex.Monster (Monster)
-- ANCHOR_END: module

-- ANCHOR: files
-- 📖 Read the bestiary. No file yet: an empty bestiary.
-- 図鑑を読む。ファイルがまだなければ、空の図鑑
loadBestiary :: FilePath -> IO [Monster]
loadBestiary path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Right monsters -> pure monsters
        -- 「mondex: （ファイル名）が読めない：（理由）」
        Left err -> die ("mondex: cannot read " ++ path ++ ": " ++ err)

-- 💾 Write the bestiary as pretty JSON.
-- 図鑑を、読みやすい JSON で書く
saveBestiary :: FilePath -> [Monster] -> IO ()
saveBestiary path monsters = BL.writeFile path (encodePretty monsters)
-- ANCHOR_END: files
-- ANCHOR_END: all
