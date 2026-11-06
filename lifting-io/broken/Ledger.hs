-- ANCHOR: all
-- | This does NOT compile: withFile from base wants an IO step,
--   but we give it an App step.
-- これはコンパイルできない：base の withFile は IO の手順が欲しいのに、
-- App の手順を渡している
module Ledger (recordBroken) where

import Data.Text (Text)
import System.IO (IOMode (..), withFile)

import Lift.App
import Lift.Ledger (writeEntry)

-- ANCHOR: broken
recordBroken :: FilePath -> [Text] -> App ()
recordBroken path entries =
  withFile path AppendMode $ \h ->
    mapM_ (writeEntry h) entries
-- ANCHOR_END: broken
-- ANCHOR_END: all
