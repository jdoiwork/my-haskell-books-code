-- | The minimq server, with a journal and a supervised acceptor.
-- minimq のサーバー。日誌つきで、受付係は監督役の下で動く
module Main (main) where

import MiniMq.Server
import MiniMq.Supervisor

-- ANCHOR: main
main :: IO ()
main = withEnv (Just "minimq.journal") $ \env -> do
  server <- listenOn 5673
  -- 「minimq：5673 番で待っています（日誌は minimq.journal）」
  putStrLn "minimq: listening on 127.0.0.1:5673 (journal: minimq.journal)"
  withGuests $ \welcome -> supervise Plan
    { strategy  = OneForOne
    , intensity = Intensity { restarts = 5, seconds = 10 }
    , children  = [Child "acceptor" (acceptLoop env server welcome)]
    }
-- ANCHOR_END: main
