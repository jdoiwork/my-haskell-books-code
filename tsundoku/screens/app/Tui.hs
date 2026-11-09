-- ANCHOR: all
-- 🖥️ The shelf in the terminal. It asks the API of a running
--    `shelf serve` (TSUNDOKU_API, http://localhost:8080/api if unset).
-- ターミナルの本棚。動いている shelf serve の API に聞く
-- （TSUNDOKU_API。なければ http://localhost:8080/api）
module Main (main) where

import Brick (defaultMain)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Servant.Client (parseBaseUrl)
import System.Environment (lookupEnv)

import Tsundoku.Api.Client (remoteApi)
import Tsundoku.Tui.Model (initial)
import Tsundoku.Tui.Run (tuiApp)

main :: IO ()
main = do
  url <- fromMaybe "http://localhost:8080/api" <$> lookupEnv "TSUNDOKU_API"
  env <- remoteApi =<< parseBaseUrl url
  void (defaultMain (tuiApp env) initial)
-- ANCHOR_END: all
