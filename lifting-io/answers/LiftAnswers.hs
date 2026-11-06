-- ANCHOR: all
-- | Answers to the training of the chapter.
-- この章のトレーニングの解答
module LiftAnswers
  ( logLines
  , withShop
  , shopDay
  ) where

import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import UnliftIO (MonadUnliftIO (..))

import Lift.App

-- ANCHOR: logLines
-- 🔢 Print the lines with numbers, in any monad that can do IO.
-- 番号を付けて、何行かを出す。IO ができるモナドなら、どこでも
logLines :: MonadIO m => [Text] -> m ()
logLines messages =
  for_ (zip [1 :: Int ..] messages) $ \(n, msg) ->
    liftIO (TIO.putStrLn (T.pack (show n) <> ": " <> msg))
-- ANCHOR_END: logLines

-- ANCHOR: withShop
-- 🏪 Open the shop, and close it whatever happens. bracket_ from base
--    takes IO steps, so we turn the App steps into IO with run.
-- 店を開け、何があっても閉める。base の bracket_ は IO の手順を受け取るので、
-- run で App の手順を IO にする
withShop :: App a -> App a
withShop body =
  withRunInIO $ \run ->
    bracket_ (run (logInfo "shop: open"))
             (run (logInfo "shop: closed"))
             (run body)

-- 🧪 A day at the shop. / 店の1日
shopDay :: App ()
shopDay = withShop (logInfo "sold: herb")
-- ANCHOR_END: withShop
-- ANCHOR_END: all
