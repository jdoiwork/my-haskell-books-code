-- | The minimq server, with the acceptor under a supervisor.
-- minimq のサーバー。受付係は監督役の下で動く
module Main (main) where

import MiniMq.Server
import MiniMq.Supervisor

-- ANCHOR: main
main :: IO ()
main = do
  env <- newEnv
  server <- listenOn 5673
  -- 「minimq：5673 番で待っています」
  putStrLn "minimq: listening on 127.0.0.1:5673"
  supervise Plan
    { strategy  = OneForOne
    , intensity = Intensity { restarts = 5, seconds = 10 }
    , children  = [Child "acceptor" (acceptLoop env server)]
    }
-- ANCHOR_END: main
