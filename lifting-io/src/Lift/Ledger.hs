-- ANCHOR: all
-- | The other way round: functions that take IO steps (withFile,
--   forConcurrently). We pass runApp env in by hand.
-- 逆向き：IO の手順を受け取る関数（withFile、forConcurrently）。
-- runApp env を、手ではさむ
module Lift.Ledger
  ( writeEntry
  , recordByHand
  , appraiseAllByHand
  ) where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (Handle, IOMode (..), withFile)

import Lift.App

-- ANCHOR: entry
-- ✍️ Write one entry in the ledger, and log it (an App step).
-- 帳簿に1行書いて、ログにも残す（App の手順）
writeEntry :: Handle -> Text -> App ()
writeEntry h entry = do
  liftIO (TIO.hPutStrLn h entry)
  logInfo ("recorded: " <> entry)
-- ANCHOR_END: entry

-- ANCHOR: byHand
-- 📒 Open the ledger, write, and close it, even if something fails.
--    withFile wants an IO step, so we turn the App step into IO by hand.
-- 帳簿を開いて書き、何があっても閉じる。withFile は IO の手順が欲しいので、
-- App の手順を、手で IO にする
recordByHand :: FilePath -> [Text] -> App ()
recordByHand path entries = do
  env <- ask
  liftIO $ withFile path AppendMode $ \h ->
    runApp env (mapM_ (writeEntry h) entries)

-- 🔍 Appraise all the items at the same time, in the same way.
-- 同じやり方で、全部の品物を同時に鑑定する
appraiseAllByHand :: [Text] -> App [Int]
appraiseAllByHand items = do
  env <- ask
  liftIO (forConcurrently items (\item -> runApp env (appraise item)))
-- ANCHOR_END: byHand
-- ANCHOR_END: all
