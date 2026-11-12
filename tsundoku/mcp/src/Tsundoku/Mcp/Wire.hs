-- ANCHOR: all
-- ANCHOR: header
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | The wire of MCP: JSON-RPC, one message per line, on stdin and stdout.
--   A tool is a value (its name, what it does, the JSON Schema of its
--   arguments, and the function that runs it); the server answers
--   initialize, tools/list, and tools/call with whatever tools it is given.
-- MCP の線：JSON-RPC。1行に1つのメッセージを、標準入力と標準出力で。
-- 道具は値（名前、すること、引数の JSON Schema、動かす関数）。サーバーは、
-- 渡された道具で、initialize・tools/list・tools/call に答える
module Tsundoku.Mcp.Wire
  ( Tool (..)
  , Reply (..)
  , handle
  , serveStdio
  , arg
  , Arg (..)
  , argOf
  ) where

-- exceptions, JSON, bytes, text, and stdout
-- 例外、JSON、バイト列、文字列、標準出力
import Control.Exception (SomeAsyncException, displayException,
                          fromException, throwIO, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (BufferMode (..), hSetBuffering, stdout)
-- ANCHOR_END: header

-- ANCHOR: tool
-- 🧰 One tool: what the AI sees (name, description, schema), and what
--    runs when it is called.
-- 道具1つ：AI に見えるもの（名前、説明、スキーマ）と、呼ばれたら動くもの
data Tool = Tool
  { name        :: Text
  , description :: Text
  , schema      :: Value              -- JSON Schema of the arguments
  , run         :: Object -> IO Reply
  }

-- ✅ An answer, or a refusal with words the AI can act on.
-- 答えか、AI が読んで直せる言葉のついた断り
data Reply = Answer Text | Refuse Text
  deriving (Show, Eq)

-- 🔎 One argument, read as the type the tool wants.
-- 引数を1つ、道具が欲しい型として読む
arg :: FromJSON a => Key -> Object -> Maybe a
arg key args = parseMaybe (.: key) args

-- 🔎 The same, telling "not there" from "there, but of another type"
--    ("1" for a number): the AI can fix the second only if it is told.
-- 同じく。ただし「ない」と「あるが型がちがう」（数に "1"）を分ける。
-- 2つ目は、言われなければ AI は直せない
data Arg a = Missing | Wrong | Got a

argOf :: FromJSON a => Key -> Object -> Arg a
argOf key args = case KM.lookup key args of
  Nothing   -> Missing
  Just Null -> Missing
  Just v    -> maybe Wrong Got (parseMaybe parseJSON v)
-- ANCHOR_END: tool

-- ANCHOR: handle
-- 🧭 Answer one message with these tools. A notification (no "id")
--    gets no answer.
-- この道具たちで、メッセージに1つ答える。お知らせ（"id" がない）には
-- 答えない
handle :: Text -> [Tool] -> Value -> IO (Maybe Value)
handle server tools message =
  case parseMaybe idAndMethod message of
    Just (Just rid, method) -> Just <$> case method of
      "initialize" -> pure (success rid (hello server message))
      "ping"       -> pure (success rid (object []))
      "tools/list" -> pure (success rid toolList)
      "tools/call" -> either (uncurry (failure rid)) (success rid)
                        <$> call tools message
      _            -> pure (failure rid (-32601) "Method not found")
    _ -> pure Nothing
  where
    idAndMethod :: Value -> Parser (Maybe Value, Text)
    idAndMethod = withObject "message" $ \o ->
      (,) <$> o .:? "id" <*> o .: "method"
    toolList = object ["tools" .= map listed tools]
    listed t = object
      [ "name" .= t.name, "description" .= t.description
      , "inputSchema" .= t.schema ]

-- 🤝 The answer to initialize: the version we both speak, and that we
--    have tools.
-- initialize への答え：おたがいに話せる版と、道具を持っていること
hello :: Text -> Value -> Value
hello server message = object
  [ "protocolVersion" .= version
  , "capabilities"    .= object ["tools" .= object []]
  , "serverInfo"      .= object [ "name"    .= server
                                , "version" .= ("0.1" :: Text) ]
  ]
  where
    asked = parseMaybe (withObject "message" $ \o ->
              o .: "params" >>= (.: "protocolVersion")) message
    version = case asked of
      Just v | v `elem` supported -> v
      _                           -> latest
    supported = ["2025-06-18", latest]
    latest    = "2025-11-25" :: Text

-- 🔧 Call the tool the AI named, with its arguments. No name, or a tool
--    we do not have, is a protocol error (the MCP spec); a refusal of the
--    tool itself is a tool result with isError.
-- AI が名指しした道具を、その引数で呼ぶ。名前がない・ない道具は、MCP の
-- 仕様のとおりプロトコルのエラー。道具そのものの断りは isError の結果
call :: [Tool] -> Value -> IO (Either (Int, Text) Value)
call tools message = case parseMaybe nameOf message of
  Nothing -> pure (Left (-32602, "params.name is missing"))
  Just wanted -> case parseMaybe argsOf message of
    Nothing -> pure (Left (-32602, "params.arguments must be an object"))
    Just args -> case [ t | t <- tools, t.name == wanted ] of
      []    -> pure (Left (-32602, "Unknown tool: " <> wanted))
      t : _ -> do
        reply <- runSafely (t.run args)
        pure $ Right $ case reply of
          Answer text -> result False text
          Refuse text -> result True  text
  where
    nameOf = withObject "message" $ \o -> o .: "params" >>= (.: "name")
    argsOf = withObject "message" $ \o -> do
      params <- o .: "params"
      params .:? "arguments" .!= mempty
    result :: Bool -> Text -> Value
    result isError text = object
      [ "content" .= [object ["type" .= ("text" :: Text), "text" .= text]]
      , "isError" .= isError ]

-- 🧯 Run a tool. If it throws (the shelf locked by another program, the
--    database gone), say so as a refusal the AI can read, and keep
--    serving: the next message still gets an answer. Ctrl-C and other
--    asynchronous exceptions go on as they are.
-- 道具を動かす。例外が飛んだら（ほかのプログラムが本棚に鍵をかけた、
-- データベースが消えた）、AI が読める断りにして、サーバーは続ける。
-- 次のメッセージにも答える。Ctrl-C などの非同期の例外は、そのまま通す
runSafely :: IO Reply -> IO Reply
runSafely act = try act >>= \case
  Right reply -> pure reply
  Left e
    | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
    | otherwise -> pure (Refuse
        ("本棚でエラーが起きました：" <> T.pack (displayException e)
           <> "。少し待って、もう一度試してください"))

-- 📮 A JSON-RPC answer, or an error.
-- JSON-RPC の答えか、エラー
success :: Value -> Value -> Value
success rid value =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= value]

failure :: Value -> Int -> Text -> Value
failure rid code text = object
  [ "jsonrpc" .= ("2.0" :: Text), "id" .= rid
  , "error"   .= object ["code" .= code, "message" .= text] ]
-- ANCHOR_END: handle

-- ANCHOR: serve
-- 🔁 Read messages line by line from stdin, answer on stdout.
-- 標準入力から1行ずつ読んで、標準出力に答える
serveStdio :: Text -> [Tool] -> IO ()
serveStdio server tools = do
  hSetBuffering stdout LineBuffering
  input <- BL.getContents
  mapM_ answer (filter (not . BL.null) (BL.lines input))
  where
    answer line = case decode line of
      Nothing      -> say (failure Null (-32700) "Parse error")
      Just message -> handle server tools message >>= mapM_ say
    say = BL.putStrLn . encode
-- ANCHOR_END: serve
-- ANCHOR_END: all
