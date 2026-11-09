{-# LANGUAGE OverloadedStrings #-}
-- ANCHOR: all
-- | A tiny MCP server: it lets an AI assistant call one tool,
--   the element chart of this book. Messages are JSON-RPC, one per
--   line on stdin and stdout.
-- 小さな MCP サーバー：AI のアシスタントが、この本の属性の相性表を
-- 道具として呼べるようにする。メッセージは JSON-RPC で、
-- 標準入力と標準出力に1行に1つ
module Modern.Mcp
  ( handle
  , handleLine
  ) where

-- ANCHOR: imports
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)

import Modern.Elements (Element (..), power)
-- ANCHOR_END: imports

-- ANCHOR: line
-- 📨 One line in, zero or one line out. Notifications get no answer.
-- 1行受け取って、0行か1行を返す。お知らせには答えない
handleLine :: BL.ByteString -> Maybe BL.ByteString
handleLine line =
  case decode line of
    Nothing      -> Just (encode (failure Null (-32700) "Parse error"))
    Just request -> encode <$> handle request
-- ANCHOR_END: line

-- ANCHOR: handle
-- 🧭 Answer one request, by its method.
-- 1つの頼みごとに、method ごとに答える
handle :: Value -> Maybe Value
handle request =
  case parseMaybe idAndMethod request of
    Nothing -> Nothing
    Just (Nothing, _) -> Nothing
    Just (Just rid, method) -> Just $ case method of
      "initialize" -> success rid (initializeResult request)
      "tools/list" -> success rid toolList
      "tools/call" -> success rid (callTool request)
      _            -> failure rid (-32601) "Method not found"
  where
    idAndMethod :: Value -> Parser (Maybe Value, Text)
    idAndMethod = withObject "request" $ \o ->
      (,) <$> o .:? "id" <*> o .: "method"
-- ANCHOR_END: handle

-- ANCHOR: initialize
-- 🤝 Agree on a protocol version and say what we can do.
-- プロトコルの版を決めて、できることを伝える
initializeResult :: Value -> Value
initializeResult request =
  object
    [ "protocolVersion" .= version
    , "capabilities"    .= object ["tools" .= object []]
    , "serverInfo"      .= object
        [ "name"    .= ("guild-mcp" :: Text)
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
-- ANCHOR_END: initialize

-- ANCHOR: tools
-- 🧰 The one tool, with a JSON Schema for its arguments.
-- 道具は1つ。引数の形を、JSON Schema で伝える
toolList :: Value
toolList =
  object
    [ "tools" .=
        [ object
            [ "name"        .= ("element_power" :: Text)
            , "description" .=
                ("Attack power in percent for an element matchup" :: Text)
            , "inputSchema" .= object
                [ "type"       .= ("object" :: Text)
                , "properties" .= object
                    [ "attacker" .= elementSchema
                    , "defender" .= elementSchema
                    ]
                , "required"   .= (["attacker", "defender"] :: [Text])
                ]
            ]
        ]
    ]
  where
    elementSchema =
      object
        [ "type" .= ("string" :: Text)
        , "enum" .= (["fire", "water", "grass"] :: [Text])
        ]

-- 🔧 Call the tool. A wrong argument is a tool error the AI can read,
--    not a protocol error.
-- 道具を呼ぶ。引数のまちがいは、AI が読める「道具のエラー」にする
--   （プロトコルのエラーにはしない）
callTool :: Value -> Value
callTool request =
  case parseMaybe arguments request of
    Just (Just attacker, Just defender) ->
      toolText False (show (power attacker defender))
    _ ->
      toolText True "attacker and defender must be fire, water or grass"
  where
    arguments = withObject "request" $ \o -> do
      args <- o .: "params" >>= (.: "arguments")
      attacker <- args .: "attacker"
      defender <- args .: "defender"
      pure (element attacker, element defender)

element :: Text -> Maybe Element
element "fire"  = Just Fire
element "water" = Just Water
element "grass" = Just Grass
element _       = Nothing

toolText :: Bool -> String -> Value
toolText isError text =
  object
    [ "content" .= [object ["type" .= ("text" :: Text), "text" .= text]]
    , "isError" .= isError
    ]
-- ANCHOR_END: tools

-- ANCHOR: reply
-- 📮 The two shapes of a JSON-RPC answer.
-- JSON-RPC の答えの、2つの形
success :: Value -> Value -> Value
success rid result =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= result]

failure :: Value -> Int -> Text -> Value
failure rid code message =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= rid
    , "error"   .= object ["code" .= code, "message" .= message]
    ]
-- ANCHOR_END: reply
-- ANCHOR_END: all
