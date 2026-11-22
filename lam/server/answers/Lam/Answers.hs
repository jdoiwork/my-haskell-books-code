-- ANCHOR: all
-- | The answers of the training of "A small web server, and one
--   program file".
-- 「小さな Web サーバーと1つの実行ファイル」のトレーニングの解答。
module Lam.Answers
  ( peekBundle
  ) where

import qualified Data.Text as T

import Lam.Bundle (Bundle (..), readBundleOf)

-- ANCHOR: peekBundle
-- 🔍 Step 3: look into a program file made by lam compile, without
--    running it: the flags baked in, and each file with its length
--    (in letters). Nothing for a file with no bundle (lam itself).
-- ステップ3：lam compile で作った実行ファイルを、動かさずにのぞく：焼き
-- こんだフラグと、ファイルごとの長さ（文字数）。包みのないファイル
-- （lam そのもの）なら Nothing
peekBundle :: FilePath -> IO (Maybe ([String], [(FilePath, Int)]))
peekBundle path = do
  found <- readBundleOf path
  pure $ case found of
    Nothing     -> Nothing
    Just (b, _) ->
      Just (bundleFlags b, [ (f, T.length t) | (f, t) <- bundleSources b ])
-- ANCHOR_END: peekBundle
-- ANCHOR_END: all
