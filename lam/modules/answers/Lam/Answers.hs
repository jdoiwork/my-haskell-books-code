-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | Step 3 of the training: the imports of a program, as a picture.
-- トレーニングのステップ3：プログラムの import を、図に。
module Lam.Answers
  ( importGraph
  , toDot
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (normalise, takeDirectory, (<.>), (</>))

import Lam.Module (Files (..))
import Lam.Parser (program, runLamParser)
import Lam.Program (Module (..), Session (..))
import Lam.Syntax

-- ANCHOR: graph
-- 🕸️ Each file (by its module's name, or by its path if it is the
--    program), and the modules it imports (the standard ones too),
--    from the program down. Each file is read once, even when two
--    files import it, and a ring of imports does not go on forever.
-- ファイルごとに（モジュールの名前で。プログラムなら道で）、import する
-- モジュール（標準のものも）。プログラムから下へたどる。2つのファイルが
-- import しても1度だけ読み、輪になっていても回り続けない
importGraph
  :: Monad m
  => Files m -> Session -> FilePath -> Text
  -> m (Either Text [(Name, [Name])])
importGraph files session file source =
  fmap (reverse . snd) <$> visit (Right ([], [])) (file, source)
  where
    visit acc (path, text) = case acc of
      Left err -> pure (Left err)
      Right (seen, edges)
        | path `elem` seen -> pure acc
        | otherwise -> case runLamParser program path text of
            Left _ -> pure (Left ("cannot read " <> T.pack path))
            Right prog -> do
              let names = map importName (programImports prog)
                  label = labelOf path prog
              found <- traverse (locate path) (filter (not . standard) names)
              foldM visit (Right (path : seen, (label, names) : edges))
                    [ f | Just f <- found ]
    -- A module by its name, a program by its path.
    -- モジュールは名前で、プログラムは道で
    labelOf path prog = case programHeader prog of
      Just (Header _ m _) -> m
      Nothing             -> T.pack path
    standard name = maybe False moduleStandard
                      (Map.lookup name (sessionModules session))
    -- The first place where the module's file is, like findModule.
    -- findModule と同じく、モジュールのファイルが最初に見つかる場所
    locate from name = firstOf
      [ normalise (dir </> T.unpack name <.> "lam")
      | dir <- takeDirectory from : searchPath files ]
    firstOf [] = pure Nothing
    firstOf (p : ps) = do
      text <- readSource files p
      maybe (firstOf ps) (\t -> pure (Just (p, t))) text
-- ANCHOR_END: graph

-- ANCHOR: dot
-- 🖼️ The graph in the DOT language of Graphviz:
--      dot -Tsvg graph.dot > graph.svg
-- Graphviz の DOT 言語で書いた図
toDot :: [(Name, [Name])] -> Text
toDot edges = T.unlines $
  ["digraph imports {"]
  <> [ "  " <> quoted f <> " -> " <> quoted m <> ";"
     | (f, ms) <- edges, m <- ms ]
  <> ["}"]
  where
    quoted t = "\"" <> t <> "\""
-- ANCHOR_END: dot
-- ANCHOR_END: all
