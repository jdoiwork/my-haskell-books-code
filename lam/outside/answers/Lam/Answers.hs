-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | The answers of the training of "To the outside world".
-- 「外の世界とつなぐ」のトレーニングの解答。
module Lam.Answers
  ( allowEnv
  , hostOf
  , allowHosts
  , readNetFlag
  ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H

import Lam.Builtins (con)
import Lam.Eval
import Lam.Io (Handler)

-- ANCHOR: allowEnv
-- 🔑 Step 1: --allow-env. lookupEnv answers a Maybe, not a Result, so
--    there is no Err to give: a variable that is not allowed looks as
--    if it were not set, and the environment looks empty.
-- ステップ1：--allow-env。lookupEnv の答えは Result ではなく Maybe
-- なので、Err を返せない。許されない変数は、ないように見せ、環境変数の
-- 一覧は空に見せる
allowEnv :: Applicative m => Bool -> Handler m -> Handler m
allowEnv allowed handle request = case request of
  LookupEnv _    | not allowed -> pure (con "Nothing" [])
  GetEnvironment | not allowed -> pure (VMap Map.empty)
  _                            -> handle request
-- ANCHOR_END: allowEnv

-- ANCHOR: allowHosts
-- 🌐 Step 2: --allow-net=pokeapi.co,www.jma.go.jp. The host of a URL,
--    read by http-client itself, so that it is the host it will visit.
-- ステップ2：URL のホスト。http-client 自身に読ませるので、本当に
-- 行く先のホストになる
hostOf :: Text -> Maybe Text
hostOf url = TE.decodeUtf8 . H.host <$> H.parseRequest (T.unpack url)

-- 🚧 Let an HTTP request through only to the hosts in the list.
-- 一覧のホストへの HTTP の頼みごとだけを通す
allowHosts :: Applicative m => [Text] -> Handler m -> Handler m
allowHosts hosts handle request = case request of
  HttpSend _ url _ _
    | maybe True (`notElem` hosts) (hostOf url) ->
        pure (con "Err" [VString (url <> ": needs --allow-net="
                                    <> maybe "?" id (hostOf url))])
  _ -> handle request

-- 🚩 "--allow-net=a,b" is Just ["a", "b"]; other flags are Nothing.
-- "--allow-net=a,b" は Just ["a", "b"]。ほかのフラグは Nothing
readNetFlag :: String -> Maybe [Text]
readNetFlag flag = case T.stripPrefix "--allow-net=" (T.pack flag) of
  Just hosts -> Just (filter (not . T.null) (T.splitOn "," hosts))
  Nothing    -> Nothing
-- ANCHOR_END: allowHosts
-- ANCHOR_END: all
