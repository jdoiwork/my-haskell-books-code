-- | Time the appraisals: one after another, by hand, and with unliftio.
-- 鑑定の時間を測る：1つずつ、手ではさむ版、unliftio の版
module Main (main) where

import Data.Text (Text)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import Lift.App
import Lift.Ledger (appraiseAllByHand)
import Lift.Unlift (appraiseAll)

items :: [Text]
items = ["sword", "shield", "herb", "potion", "ring"]

-- ⏱️ Run an App, and print the answer and how long it took.
-- App を動かして、答えとかかった時間を出す
timed :: String -> App [Int] -> IO ()
timed name app = do
  start  <- getCurrentTime
  prices <- runApp guildEnv app
  end    <- getCurrentTime
  let secs = realToFrac (diffUTCTime end start) :: Double
  printf "%-8s %s %.1f s\n" name (show prices) secs

main :: IO ()
main = do
  timed "mapM" (mapM appraise items)
  timed "byHand" (appraiseAllByHand items)
  timed "unliftio" (appraiseAll items)
