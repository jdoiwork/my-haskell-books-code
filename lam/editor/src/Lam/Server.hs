-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | A small web server for Lam: Http.serve 8080 handler. The builtin
--   only makes a sign (Serve). The real runner listens with warp, and
--   does the handler for each request as a job of its own; a fake
--   world gives the requests it has, and keeps the answers.
-- Lam の小さな Web サーバー：Http.serve 8080 handler。組み込みの関数は
-- 合図（Serve）を作るだけ。本物の実行役は warp で待ち、頼みごとごとに
-- handler を自分の仕事としてやる。偽物の世界は、持っている頼みを渡し、
-- 答えを取っておく
module Lam.Server
  ( serverBuiltins
  , WebRequest (..)
  , webRequest
  , webValue
  , requestValue
  , answering
  , fakeServe
  , serveWarp
  ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (race)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv)

import Lam.Builtins
import Lam.Eval
import Lam.Types
-- ANCHOR_END: head

-- ANCHOR: table
-- 📋 Http.serve : Int -> (Request -> IO Response) -> IO ()
--    The handler is a Lam function; for each request it gives an IO
--    action (ioOf), done later by the runner.
-- handler は Lam の関数。頼みごとごとに IO の頼みを作り、実行役があとで
-- やる
serverBuiltins :: [Builtin]
serverBuiltins =
  [ prim2 "Http.serve" (TInt ~> (tRequest ~> tIO tResponse) ~> tIO tUnit) $
      \p f -> case p of
        VInt n | n >= 0 && n < 65536 ->
          Right (VIO (Serve (fromInteger n) (\req -> apply f req >>= io)))
        _ -> Left (BuiltinFailed "Http.serve" "needs a port, 0 to 65535")
  ]
  where
    io v = case v of
      VIO action -> Right action
      _ -> Left (BuiltinFailed "Http.serve" "the handler did not give IO")
    -- ANCHOR: requestType
    -- A request from the web is a record, too: the method, the path
    -- (without ?...), the query (?q=lam), the headers (their names in
    -- small letters), and the body.
    -- Web からの頼みも、レコード：メソッド、道（?… なし）、クエリ
    -- （?q=lam）、ヘッダー（名前は小文字）、本文
    tRequest = TRecord (Map.fromList
      [ ("method", tString)
      , ("path", tString)
      , ("query", tMap tString tString)
      , ("headers", tMap tString tString)
      , ("body", tString) ]) Nothing
    -- ANCHOR_END: requestType
    -- ANCHOR: responseType
    -- The answer: the same record as the answers of Http.send. Its
    -- body is bytes, so it can be an image too.
    -- 答え：Http.send の答えと同じレコード。本文はバイト列なので、
    -- 画像も返せる
    tResponse = TRecord (Map.fromList
      [ ("status", TInt)
      , ("headers", tMap tString tString)
      , ("body", TCon "Bytes" []) ]) Nothing
    -- ANCHOR_END: responseType
-- ANCHOR_END: table

-- ANCHOR: webRequest
-- 🌐 A request from the web, as Haskell keeps it (a fake world has a
--    list of them): the method, the path, the query, the headers, and
--    the body.
-- Web からの頼みを、Haskell で持つ形（偽物の世界は、これの一覧を持つ）：
-- メソッド、道、クエリ、ヘッダー、本文
data WebRequest = WebRequest
  { webMethod  :: Text
  , webPath    :: Text
  , webQuery   :: [(Text, Text)]
  , webHeaders :: [(Text, Text)]
  , webBody    :: Text
  } deriving (Eq, Show)

-- 🌐 GET /todos?done=yes : the path is cut at ?, and the query is read
--    (no %xx in a fake world).
-- 道は ? で切り、クエリを読む（偽物の世界には %xx はない）
webRequest :: Text -> Text -> Text -> WebRequest
webRequest method target body =
  let (path, rest) = T.breakOn "?" target
      pairs = filter (not . T.null) (T.splitOn "&" (T.drop 1 rest))
      pair p = let (k, v) = T.breakOn "=" p in (k, T.drop 1 v)
  in WebRequest method path (map pair pairs)
       [("content-type", "application/json") | not (T.null body)] body

webValue :: WebRequest -> Value
webValue r =
  requestValue (webMethod r) (webPath r) (webQuery r) (webHeaders r)
    (webBody r)
-- ANCHOR_END: webRequest

-- ANCHOR: requestValue
-- 🧱 The record of a request, as the runner makes it.
-- 頼みのレコード。実行役が作る
requestValue
  :: Text -> Text -> [(Text, Text)] -> [(Text, Text)] -> Text -> Value
requestValue method path query headers body = VRecord (Map.fromList
  [ ("method", VString method)
  , ("path", VString path)
  , ("query", strings query)
  , ("headers", strings headers)
  , ("body", VString body) ])
  where
    strings kvs = VMap (Map.fromList
      [ (Key (VString k), VString v) | (k, v) <- kvs ])
-- ANCHOR_END: requestValue

-- ANCHOR: answering
-- 🛎️ What one job of the server does: the handler, then write down
--    the answer (Served). If the handler stops with an error, the
--    answer is 500, and the error goes to the standard error: one bad
--    request does not stop the server. exit stops it (IO.try lets
--    exit go on out).
-- サーバーの仕事1つがすること：handler をやり、答えを書き留める
-- （Served）。handler がエラーで止まったら、答えは 500 にして、エラーは
-- 標準エラーへ。まちがった頼み1つで、サーバーは止まらない。exit なら
-- 止まる（IO.try は exit を外へ通す）
answering :: (Value -> Either EvalError Action) -> Value -> Action
answering handler req =
  Bind (Catch body) $ \tried -> Right $ case tried of
    VCon "Ok" _ [resp] -> served resp
    VCon "Err" _ [VString why] ->
      Bind (Ask (PutErr (method <> " " <> path <> ": " <> why <> "\n")))
        (\_ -> Right (served (errorResponse 500 "internal server error")))
    _ -> served (errorResponse 500 "internal server error")
  where
    body = case handler req of
      Right action -> action
      Left err     -> Bind (Pure (VTuple [])) (\_ -> Left err)
    method = field "method"
    path   = field "path"
    field name = case req of
      VRecord m | Just (VString t) <- Map.lookup name m -> t
      _ -> ""
    served resp =
      let (status, _, body) = responseParts resp
          exchange = Exchange method path status (forPeople body)
      in Bind (Ask (Served exchange)) (\_ -> Right (Pure resp))
-- ANCHOR_END: answering

-- ANCHOR: forPeople
-- 👀 The body of an answer, for the record of the exchanges (and the
--    tests): the text if it is UTF-8, or how many bytes it is.
-- 答えの本文を、やりとりの記録（とテスト）のために：UTF-8 なら文字、
-- そうでなければ何バイトか
forPeople :: B.ByteString -> Text
forPeople body = case TE.decodeUtf8' body of
  Right text -> text
  Left _     -> "<" <> T.pack (show (B.length body)) <> " bytes>"
-- ANCHOR_END: forPeople

-- ANCHOR: responseParts
-- 🔍 The status, the headers, and the body of a Response record. A
--    status that is not 100 to 599 becomes 500.
-- Response のレコードの状態コード・ヘッダー・本文。100〜599 でない
-- 状態コードは 500 にする
responseParts :: Value -> (Int, [(Text, Text)], B.ByteString)
responseParts v = case v of
  VRecord m ->
    let status = case Map.lookup "status" m of
          Just (VInt n) | n >= 100 && n <= 599 -> fromInteger n
          _ -> 500
        headers = case Map.lookup "headers" m of
          Just (VMap hs) -> [ (k, t) | (Key (VString k), VString t)
                                         <- Map.toList hs ]
          _ -> []
        body = case Map.lookup "body" m of
          Just (VBytes b) -> b
          _ -> B.empty
    in (status, headers, body)
  _ -> (500, [], B.empty)

-- 🧯 A plain answer of our own: 500 when the handler failed.
-- 自分で作る、ただの答え：handler が失敗したときの 500
errorResponse :: Int -> Text -> Value
errorResponse status text = VRecord (Map.fromList
  [ ("status", VInt (toInteger status))
  , ("headers", VMap (Map.singleton (Key (VString "content-type"))
                        (VString "text/plain; charset=utf-8")))
  , ("body", VBytes (TE.encodeUtf8 (text <> "\n"))) ])
-- ANCHOR_END: responseParts

-- ANCHOR: fakeServe
-- 🎭 Serve in a fake world, made of signs the scheduler knows already:
--    ask to listen, ask for the requests the world has (each with when
--    it comes, in ms after the start), and do them all at once, each
--    when it comes. When they are all answered, serve ends (a fake
--    server does not wait for ever).
-- 偽物の世界の serve を、スケジューラーがもう知っている合図で作る：
-- 待ってよいかを聞き、世界が持っている頼み（それぞれ、始まってから何
-- ミリ秒後に来るか）をもらい、全部を同時に、それぞれ来たときにやる。
-- 全部に答えたら serve は終わる（偽物のサーバーは、いつまでも待たない）
fakeServe :: Int -> (Value -> Either EvalError Action) -> Action
fakeServe port handler =
  Bind (Ask (Listen port)) $ \allowed -> case allowed of
    VCon "Err" _ [VString why] -> Left (BuiltinFailed "Http.serve" why)
    _ -> Right $ Bind (Ask (Incoming port)) $ \coming -> case coming of
      VList reqs -> Right $
        Bind (Gather 0 WaitAll (map job reqs)) (\_ -> Right (Pure unit))
      _ -> Left (BuiltinFailed "Http.serve" "a bad list of requests")
  where
    job r = case r of
      VTuple [VInt ms, req] ->
        Bind (Ask (Arrive ms)) (\_ -> Right (answering handler req))
      _ -> answering handler r
    unit = VTuple []
-- ANCHOR_END: fakeServe

-- ANCHOR: serveWarp
-- 🌐 Serve in the real world, with warp: listen on localhost only.
--    run gets a request and does the job (answering), on the thread
--    warp gave to it: requests that come at once run at once. An exit
--    in a job stops the whole server: warp is cancelled (race), and
--    serve ends with that stop.
-- 本物の世界の serve を warp で：待つのは localhost だけ。run は頼みを
-- 受け取って仕事（answering）をやる。warp がくれたスレッドの上で
-- やるので、同時に来た頼みは同時に動く。仕事の中で exit したら、
-- サーバーまるごと止める：warp を取り消し（race）、serve はその止まり方で
-- 終わる
serveWarp
  :: (Value -> IO (Either Stop Value)) -> (Request -> IO Value) -> Int
  -> IO (Either Stop Value)
serveWarp run handle port = do
  allowed <- handle (Listen port)
  case allowed of
    VCon "Err" _ [VString why] ->
      pure (Left (Failed (BuiltinFailed "Http.serve" why)))
    _ -> do
      stopped <- newEmptyMVar
      -- ANCHOR: host
      -- Only this machine, unless LAM_HOST says more: in a container,
      -- LAM_HOST=0.0.0.0 lets the requests from outside it in.
      -- このマシンだけ。LAM_HOST があればそれ：コンテナーの中では、
      -- LAM_HOST=0.0.0.0 で、外からの頼みを入れる
      host <- maybe "127.0.0.1" fromString <$> lookupEnv "LAM_HOST"
      -- ANCHOR_END: host
      let settings = Warp.setPort port
                   $ Warp.setHost host
                   $ Warp.setBeforeMainLoop (listening handle port)
                   $ Warp.defaultSettings
          app wreq respond = do
            req <- fromWai wreq
            answer <- run req
            case answer of
              Right resp -> respond (toWai resp)
              Left stop -> do
                _ <- tryPutMVar stopped stop
                respond (toWai (errorResponse 503 "the server is stopping"))
      ended <- race (Warp.runSettings settings app) (takeMVar stopped)
      pure (either (\() -> Right (VTuple [])) Left ended)

-- 📣 Say where we listen (on the standard error, as Deno does).
-- どこで待っているかを言う（Deno と同じく、標準エラーに）
listening :: (Request -> IO Value) -> Int -> IO ()
listening handle port = () <$ handle (PutErr
  ("listening on http://localhost:" <> T.pack (show port) <> "/\n"))
-- ANCHOR_END: serveWarp

-- ANCHOR: wai
-- 🔄 A request of wai, as Lam's record: the bytes become text (a bad
--    UTF-8 byte becomes U+FFFD).
-- wai の頼みを、Lam のレコードに：バイト列は文字に（まちがった UTF-8 の
-- バイトは U+FFFD に）
fromWai :: Wai.Request -> IO Value
fromWai wreq = do
  body <- Wai.strictRequestBody wreq
  pure (requestValue
    (text (Wai.requestMethod wreq))
    (text (HT.urlDecode False (Wai.rawPathInfo wreq)))
    [ (text k, maybe "" text v) | (k, v) <- Wai.queryString wreq ]
    [ (T.toLower (text (CI.original k)), text v)
    | (k, v) <- Wai.requestHeaders wreq ]
    (text (BL.toStrict body)))
  where
    -- The path is still in %xx; wai has read the query already.
    -- 道は %xx のまま。クエリは wai がもう読んである
    text = TE.decodeUtf8With TE.lenientDecode

-- 🔄 Lam's Response record, as a response of wai.
-- Lam の Response のレコードを、wai の答えに
toWai :: Value -> Wai.Response
toWai resp =
  let (status, headers, body) = responseParts resp
  in Wai.responseLBS (toEnum status)
       [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v) | (k, v) <- headers ]
       (BL.fromStrict body)
-- ANCHOR_END: wai
-- ANCHOR_END: all
