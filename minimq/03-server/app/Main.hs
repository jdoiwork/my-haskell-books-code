-- | The minimq server. / minimq のサーバー
module Main (main) where

import MiniMq.Server

main :: IO ()
main = do
  env <- newEnv
  server <- listenOn 5673
  -- 「minimq：5673 番で待っています」
  putStrLn "minimq: listening on 127.0.0.1:5673"
  acceptLoop env server
