{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | A small kit for MCP servers: a tool is a value, and one loop serves
--   any list of tools. The AI chapter's guild-mcp, grown up.
-- MCP サーバーの小さな道具箱：道具は値で、1つのループが、
-- どんな道具のリストでも受け持つ。AI の章の guild-mcp を育てたもの
module Modern.McpKit
  ( Tool (..)
  , ToolResult (..)
  , handleWith
  , serve
  ) where

-- ANCHOR: imports
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (find)
import Data.Text (Text)
import System.IO (BufferMode (..), hSetBuffering, stdout)
-- ANCHOR_END: imports

-- ANCHOR: tool
-- 🧰 A tool: its name, what it does, the shape of its arguments
--    (JSON Schema), and the work itself.
-- 道具：名前、何をするか、引数の形（JSON Schema）、そして仕事そのもの
data Tool = Tool
  { toolName        :: Text
  , toolDescription :: Text
  , toolArguments   :: [(Key, Value)]
  , toolRequired    :: [Text]
  , toolRun         :: Object -> IO ToolResult
  }

-- ✅ The answer, or a refusal the AI can read and act on.
-- 答えか、AI が読んで直せる「断り」
data ToolResult
  = Answer Text
  | Refuse Text
-- ANCHOR_END: tool

-- ANCHOR: handle
-- 🧭 Answer one request with the given tools. The name of the server and
--    the list of tools are the only things that change from server to
--    server.
-- 渡された道具で、頼みごとに1つ答える。サーバーごとに変わるのは、
-- サーバーの名前と道具のリストだけ
handleWith :: Text -> [Tool] -> Value -> IO (Maybe Value)
handleWith server tools request =
  case parseMaybe idAndMethod request of
    Nothing                 -> pure Nothing
    Just (Nothing, _)       -> pure Nothing
    Just (Just rid, method) ->
      Just <$> case method of
        "initialize" -> pure (success rid (hello server request))
        "ping"       -> pure (success rid (object []))
        "tools/list" -> pure (success rid (listOf tools))
        "tools/call" -> success rid <$> call tools request
        _            -> pure (failure rid (-32601) "Method not found")
  where
    idAndMethod :: Value -> Parser (Maybe Value, Text)
    idAndMethod = withObject "request" $ \o ->
      (,) <$> o .:? "id" <*> o .: "method"
-- ANCHOR_END: handle

-- ANCHOR: list
-- 📋 The list of tools, made from the values.
-- 道具の一覧を、値から作る
listOf :: [Tool] -> Value
listOf tools = object ["tools" .= map describe tools]
  where
    describe t =
      object
        [ "name"        .= toolName t
        , "description" .= toolDescription t
        , "inputSchema" .= object
            [ "type"       .= ("object" :: Text)
            , "properties" .= object [ k .= v | (k, v) <- toolArguments t ]
            , "required"   .= toolRequired t
            ]
        ]

-- 🔧 Find the tool by name and run it.
-- 名前で道具を探して、動かす
call :: [Tool] -> Value -> IO Value
call tools request =
  case parseMaybe nameAndArguments request of
    Nothing -> pure (result (Refuse "params.name is missing"))
    Just (name, arguments) ->
      case find (\t -> toolName t == name) tools of
        -- 「そんな道具はありません」
        Nothing -> pure (result (Refuse ("no such tool: " <> name)))
        Just t  -> result <$> toolRun t arguments
  where
    nameAndArguments :: Value -> Parser (Text, Object)
    nameAndArguments = withObject "request" $ \o -> do
      params <- o .: "params"
      (,) <$> params .: "name" <*> params .:? "arguments" .!= KM.empty
    result r =
      let (isError, text) = case r of
            Answer t -> (False, t)
            Refuse t -> (True, t)
          part = object ["type" .= ("text" :: Text), "text" .= text]
      in object ["content" .= [part], "isError" .= isError]
-- ANCHOR_END: list

-- ANCHOR: reply
-- 🤝 Agree on a version, as in the AI chapter.
-- AI の章と同じく、版を合わせる
hello :: Text -> Value -> Value
hello server request =
  object
    [ "protocolVersion" .= version
    , "capabilities"    .= object ["tools" .= object []]
    , "serverInfo"      .= object
        [ "name"    .= server
        , "version" .= ("0.1.0" :: Text)
        ]
    ]
  where
    asked = parseMaybe (withObject "request" $ \o ->
              o .: "params" >>= (.: "protocolVersion")) request
    version
      | asked `elem` map Just supported = maybe latest id asked
      | otherwise                       = latest
    supported = ["2025-06-18", "2025-11-25"] :: [Text]
    latest    = "2025-11-25"

-- 📮 The two shapes of a JSON-RPC answer.
-- JSON-RPC の答えの、2つの形
success :: Value -> Value -> Value
success rid r =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= r]

failure :: Value -> Int -> Text -> Value
failure rid code message =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= rid
    , "error"   .= object ["code" .= code, "message" .= message]
    ]
-- ANCHOR_END: reply

-- ANCHOR: serve
-- 🔁 Read a message from each line of stdin, answer on stdout.
-- 標準入力の1行ごとにメッセージを読み、標準出力に答える
serve :: Text -> [Tool] -> IO ()
serve server tools = do
  hSetBuffering stdout LineBuffering
  input <- BL.getContents
  mapM_ answer (filter (not . BL.null) (BL.lines input))
  where
    answer line =
      case decode line of
        Nothing -> say (failure Null (-32700) "Parse error")
        Just request -> do
          reply <- handleWith server tools request
          mapM_ say reply
    say = BL.putStrLn . encode
-- ANCHOR_END: serve
-- ANCHOR_END: all
