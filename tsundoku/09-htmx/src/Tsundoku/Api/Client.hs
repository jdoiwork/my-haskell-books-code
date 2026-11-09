-- ANCHOR: all
-- ANCHOR: header
-- | The client: the same records of routes, made into functions that
--   ask a server over HTTP. A program in Haskell (the terminal screen,
--   later) uses the API through this, with no JSON written by hand.
-- クライアント：同じ道のレコードを、HTTP でサーバーに聞く関数にしたもの。
-- Haskell のプログラム（あとの章の、端末の画面）は、これを通して API を
-- 使う。JSON を手で書くことはない
module Tsundoku.Api.Client
  ( shelfClient
  , localApi
  , call
  , problemOf
  ) where

-- HTTP connections, and JSON
-- HTTP の接続と、JSON
import Data.Aeson (decode)
import Network.HTTP.Client (defaultManagerSettings, newManager)

-- servant: the client side, from records of routes
-- servant：クライアントの側を、道のレコードから
import Servant.Client

import Tsundoku.Api.Json (Problem)
import Tsundoku.Api.Routes
-- ANCHOR_END: header

-- ANCHOR: client
-- 📞 Every route as a function. shelfClient.listWorks asks for the list;
--    (shelfClient.work 3).card asks for the card of work 3.
-- 道の全部を関数に。shelfClient.listWorks は一覧を、
-- (shelfClient.work 3).card は作品 3 のカードを聞く
shelfClient :: ShelfRoutes (AsClientT ClientM)
shelfClient = client shelfApi

-- 🔌 A server on this machine, at this port.
-- このマシンの、このポートのサーバー
localApi :: Int -> IO ClientEnv
localApi port = do
  manager <- newManager defaultManagerSettings
  pure (mkClientEnv manager (BaseUrl Http "localhost" port ""))

-- ▶️ Ask the server.
-- サーバーに聞く
call :: ClientEnv -> ClientM a -> IO (Either ClientError a)
call env ask = runClientM ask env

-- 🚫 Why the server said no, if it said so with a Problem.
-- サーバーが断った理由（Problem で言っていれば）
problemOf :: ClientError -> Maybe Problem
problemOf failure = case failure of
  FailureResponse _ response -> decode (responseBody response)
  _                          -> Nothing
-- ANCHOR_END: client
-- ANCHOR_END: all
