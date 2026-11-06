-- ANCHOR: all
-- 🌐 Start the quest board server on port 8080, with board.db.
-- 依頼の掲示板のサーバーを、board.db を使って 8080 番で立ち上げる
module Main (main) where

import Database.SQLite.Simple   (withConnection)
import Network.Wai.Handler.Warp (run)

import Board.Api   (app)
import Board.Store (migrate)

main :: IO ()
main =
  withConnection "board.db" $ \conn -> do
    migrate conn
    -- 「http://localhost:8080 で待っています」
    putStrLn "Listening on http://localhost:8080"
    run 8080 (app conn)
-- ANCHOR_END: all
