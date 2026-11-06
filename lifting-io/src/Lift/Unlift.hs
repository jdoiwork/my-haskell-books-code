-- ANCHOR: all
-- | MonadUnliftIO: ask App for "a way to run App as IO", and use the
--   functions that take IO steps from inside App.
-- MonadUnliftIO：App に「App を IO として動かす方法」をもらって、
-- IO の手順を受け取る関数を、App の中から使う
module Lift.Unlift
  ( recordWith
  , record
  , appraiseAll
  , Dragon (..)
  , withDesk
  , deskTrouble
  , callbackFor
  ) where

import Data.Text (Text)
import qualified System.IO as IO

-- ANCHOR: imports
-- unliftio: the same tools as base and async, for any MonadUnliftIO.
-- unliftio：base や async と同じ道具を、MonadUnliftIO ならどれでも
import UnliftIO (MonadUnliftIO (..), askRunInIO)
import UnliftIO.Async (forConcurrently)
import UnliftIO.Exception (Exception, bracket_, throwIO, try)
import UnliftIO.IO (IOMode (..), withFile)
-- ANCHOR_END: imports

import Lift.App
import Lift.Ledger (writeEntry)

-- ANCHOR: withRunInIO
-- 📒 withRunInIO hands us run, which turns an App step into IO.
-- withRunInIO が run をくれる。run は App の手順を IO にする
recordWith :: FilePath -> [Text] -> App ()
recordWith path entries =
  withRunInIO $ \run ->
    IO.withFile path AppendMode $ \h ->
      run (mapM_ (writeEntry h) entries)
-- ANCHOR_END: withRunInIO

-- ANCHOR: unliftio
-- 📒 The unliftio withFile takes the App step as it is.
-- unliftio の withFile は、App の手順をそのまま受け取る
record :: FilePath -> [Text] -> App ()
record path entries =
  withFile path AppendMode $ \h ->
    mapM_ (writeEntry h) entries

-- 🔍 So does forConcurrently.
-- forConcurrently も同じ
appraiseAll :: [Text] -> App [Int]
appraiseAll items = forConcurrently items appraise
-- ANCHOR_END: unliftio

-- ANCHOR: desk
-- 🐉 Something that can go wrong at the desk.
-- 受付で起きるかもしれない困りごと
data Dragon = Dragon
  deriving (Show)

instance Exception Dragon

-- 🚪 Open the desk, and close it whatever happens.
-- 受付を開け、何があっても閉める
withDesk :: App a -> App a
withDesk = bracket_ (logInfo "desk: open") (logInfo "desk: closed")

-- 🧪 A dragon comes in while the desk is open.
-- 受付が開いている間に、ドラゴンが来た
deskTrouble :: App (Either Dragon ())
deskTrouble = try (withDesk (logInfo "a dragon!" >> throwIO Dragon))
-- ANCHOR_END: desk

-- ANCHOR: callback
-- 📞 Make an IO callback out of an App step, to hand to IO code later.
-- App の手順から、IO のコールバックを作る。あとで IO のコードに渡すため
callbackFor :: Text -> App (IO ())
callbackFor name = do
  run <- askRunInIO
  pure (run (logInfo ("called back: " <> name)))
-- ANCHOR_END: callback
-- ANCHOR_END: all
