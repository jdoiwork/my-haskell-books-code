-- ANCHOR: all
{-# LANGUAGE OverloadedStrings #-}
-- | lam lsp: a small language server. An editor sends it messages of
--   JSON-RPC (the Language Server Protocol) on the standard input, and
--   it answers on the standard output: the red lines (diagnostics) and
--   the types under the mouse (hover).
-- lam lsp：小さな言語サーバー。エディターが JSON-RPC（Language Server
-- Protocol）の知らせを標準入力に送り、サーバーは標準出力に答える。赤い線
-- （診断）と、マウスの下の型（ホバー）
module Lam.Lsp
  ( Server (..)
  , newServer
  , step
  , readMessage
  , writeMessage
  , serve
  , uriPath
  , toCharacter
  , toColumn
  ) where

-- ANCHOR: head
import Control.Monad (foldM, forM_, unless)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Char (digitToInt, isHexDigit, ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeBaseName, takeFileName)
import System.IO (Handle, hFlush, hIsEOF, hSetBinaryMode)
import Text.Megaparsec.Unicode (stringLength)

import Lam.Check
  ( Checked (..), Problem (..), Severity (..), checkSource, columnToIndex
  , hoverAt, indexToColumn, wordAt )
import Lam.Module (Files (..))
import Lam.Program (Session)
-- ANCHOR_END: head

-- ANCHOR: server
-- 🗂️ What the server keeps between messages: the text of each open
--    file (by its URI, as the editor has it now, saved or not), the
--    session of each file that could be read (for the hover), and
--    whether the editor said exit.
-- 知らせのあいだにサーバーが覚えておくもの：開いているファイルごとの
-- 文字（URI で。エディターがいま持っているもの。保存していなくても）、
-- 読めたファイルのセッション（ホバーのため）、エディターが exit と
-- 言ったか
data Server = Server
  { serverVersion :: Text
  , openFiles     :: Map Text Text
  , sessions      :: Map Text Session
  , finished      :: Bool
  }

newServer :: Text -> Server
newServer version = Server version Map.empty Map.empty False
-- ANCHOR_END: server

-- ANCHOR: step
-- 📨 Answer one message. A request (it has an id) gets one answer;
--    a notification (no id) gets none, but opening or changing a file
--    sends the red lines of every open file again (a file may import
--    the one that changed). The files on the disk are read through
--    files; an open file is read as the editor has it.
-- 知らせ1つに答える。頼み（id がある）には答えを1つ。通知（id がない）
-- には答えないが、ファイルを開いたり変えたりしたら、開いている全部の
-- ファイルの赤い線を送り直す（変わったファイルを import しているかも
-- しれない）。ディスクのファイルは files で読み、開いているファイルは
-- エディターが持っているとおりに読む
step
  :: Monad m => Files m -> Session -> Server -> Value
  -> m (Server, [Value])
step files base server msg = case (textAt ["method"] msg, at ["id"] msg) of
  (Just "initialize", Just i) -> pure (server, [reply i (initialized server)])
  (Just "shutdown", Just i)   -> pure (server, [reply i Null])
  (Just "exit", _)            -> pure (server { finished = True }, [])
  (Just "textDocument/hover", Just i) ->
    pure (server, [reply i (hover server params)])
  (Just "textDocument/didOpen", Nothing) ->
    recheck (keep (textAt ["textDocument", "text"] params))
  (Just "textDocument/didChange", Nothing) ->
    recheck (keep (lastChange params))
  (Just "textDocument/didClose", Nothing) ->
    closed <$> recheck (forget server)
  -- 「知らない頼みです」（-32601 は、JSON-RPC の「その頼みはない」）
  (Just method, Just i) -> pure (server, [refuse i method])
  _                     -> pure (server, [])
  where
    params = fromMaybe Null (at ["params"] msg)
    uri    = fromMaybe "" (textAt ["textDocument", "uri"] params)
    keep text = server { openFiles = maybe id (Map.insert uri) text
                                       (openFiles server) }
    forget s = s { openFiles = Map.delete uri (openFiles s)
                 , sessions  = Map.delete uri (sessions s) }
    recheck s = checkAll files base s
    -- A closed file gets no red lines any more.
    -- 閉じたファイルの赤い線は消す
    closed (s, out) = (s, out <> [publish uri []])
-- ANCHOR_END: step

-- ANCHOR: initialized
-- 🤝 The answer to initialize: what this server can do. The text is
--    sent whole on each change (textDocumentSync 1), and it can hover.
-- initialize への答え：このサーバーにできること。文字は変わるたびに
-- まるごと送ってもらい（textDocumentSync 1）、ホバーができる
initialized :: Server -> Value
initialized server = object
  [ "capabilities" .= object
      [ "textDocumentSync" .= (1 :: Int)
      , "hoverProvider"    .= True ]
  , "serverInfo" .= object
      [ "name"    .= ("lam" :: Text)
      , "version" .= serverVersion server ] ]
-- ANCHOR_END: initialized

-- ANCHOR: checkAll
-- 🔁 Check every open file, and send the red lines of each.
-- 開いているファイルを全部確かめ、それぞれの赤い線を送る
checkAll
  :: Monad m => Files m -> Session -> Server -> m (Server, [Value])
checkAll files base server =
  foldM one (server { sessions = Map.empty }, [])
    (Map.toList (openFiles server))
  where
    open = withOpen (openFiles server) files
    one (s, out) (uri, text) = do
      let file = uriPath uri
      checked <- checkSource open base file text
      let lines' = map (diagnostic file text) (checkedProblems checked)
          s' = s { sessions = maybe id (Map.insert uri)
                                (checkedSession checked) (sessions s) }
      pure (s', out <> [publish uri lines'])

-- 📂 Read an open file as the editor has it; any other from the disk.
-- 開いているファイルは、エディターが持っているとおりに。ほかはディスク
withOpen :: Monad m => Map Text Text -> Files m -> Files m
withOpen open files = files { readSource = source }
  where
    byPath = Map.fromList [ (uriPath u, t) | (u, t) <- Map.toList open ]
    source path = maybe (readSource files path) (pure . Just)
                    (Map.lookup path byPath)
-- ANCHOR_END: checkAll

-- ANCHOR: diagnostic
-- 🟥 One red (or yellow) line: from the place of the problem to the
--    end of the word there. A problem in another file (one it
--    imports) goes on the line that imports it.
-- 赤い（か黄色い）線1本：まちがいの場所から、そこの語の終わりまで。
-- ほかのファイル（import したもの）のまちがいは、それを import する行に
diagnostic :: FilePath -> Text -> Problem -> Value
diagnostic file text p = object
  [ "range"    .= range
  , "severity" .= (case problemSeverity p of
                     AnError -> 1 :: Int
                     Warning -> 2)
  , "source"   .= ("lam" :: Text)
  , "message"  .= message ]
  where
    here = problemFile p == file
    (l, c)
      | here      = (problemLine p, problemColumn p)
      | otherwise = (importLine text (problemFile p), 1)
    message
      | here      = problemMessage p
      | otherwise = T.pack (takeFileName (problemFile p)) <> ":"
                      <> T.pack (show (problemLine p)) <> ":"
                      <> T.pack (show (problemColumn p)) <> ": "
                      <> problemMessage p
    width = maybe 1 (\(start, w) -> stringLength (T.unpack w) - (c - start))
                    (wordAt text l c)
    range = object [ "start" .= position text l c
                   , "end"   .= position text l (c + max 1 width) ]

-- 📏 The line that imports a file (Price.lam: import Price ...).
-- ファイルを import する行
importLine :: Text -> FilePath -> Int
importLine text other =
  fromMaybe 1 $ listToMaybe
    [ n | (n, line) <- zip [1 ..] (T.lines text)
        , ("import" : name : _) <- [T.words line]
        , name == T.pack (takeBaseName other) ]

-- 📢 The red lines of one file. An empty list takes them all away.
-- ファイル1つの赤い線。空のリストなら、全部消す
publish :: Text -> [Value] -> Value
publish uri ds = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "method"  .= ("textDocument/publishDiagnostics" :: Text)
  , "params"  .= object [ "uri" .= uri, "diagnostics" .= ds ] ]
-- ANCHOR_END: diagnostic

-- ANCHOR: hover
-- 💬 The answer to hover: the types under the place, as Markdown; or
--    null when there is nothing to show.
-- hover への答え：場所の下の型を Markdown で。見せるものがなければ null
hover :: Server -> Value -> Value
hover server params = fromMaybe Null $ do
  uri     <- textAt ["textDocument", "uri"] params
  text    <- Map.lookup uri (openFiles server)
  session <- Map.lookup uri (sessions server)
  line    <- intAt ["position", "line"] params
  char    <- intAt ["position", "character"] params
  let l = line + 1
  shown <- hoverAt session (uriPath uri) text l (toColumn text l char)
  pure (object [ "contents" .= object [ "kind"  .= ("markdown" :: Text)
                                       , "value" .= shown ] ])
-- ANCHOR_END: hover

-- ANCHOR: positions
-- 📐 LSP counts lines from 0, and the characters of a line in UTF-16
--    (🧾 is 2: it is beyond U+FFFF). lam counts both from 1, with
--    megaparsec's columns (🧾 and 日 are 2, as on a terminal). These
--    two go through the number of letters before the place.
-- LSP は行を 0 から、行の中の文字を UTF-16 で数える（🧾 は U+FFFF より
-- 先なので2つ）。lam はどちらも 1 から、megaparsec の桁で数える（🧾 も
-- 日も、端末と同じく2桁）。この2つは、その場所より前の文字の数を通る
position :: Text -> Int -> Int -> Value
position text l c =
  object [ "line" .= (l - 1), "character" .= toCharacter text l c ]

-- lam's column (from 1) → LSP's character (UTF-16, from 0)
toCharacter :: Text -> Int -> Int -> Int
toCharacter text l c =
  sum (map units (T.unpack (T.take (columnToIndex line c) line)))
  where
    line = lineOf text l

-- LSP's character (UTF-16, from 0) → lam's column (from 1)
toColumn :: Text -> Int -> Int -> Int
toColumn text l char = indexToColumn line (go 0 0 (T.unpack line))
  where
    line = lineOf text l
    go i used rest = case rest of
      ch : more | used + units ch <= char -> go (i + 1) (used + units ch) more
      _ -> i

-- 📄 Line l of a text (from 1).
-- 文字の l 行目（1 から）
lineOf :: Text -> Int -> Text
lineOf text l = fromMaybe "" (listToMaybe (drop (l - 1) (T.lines text)))

-- 🔢 A character beyond U+FFFF takes two units of UTF-16.
-- U+FFFF より先の文字は、UTF-16 で2つ分
units :: Char -> Int
units ch = if ord ch > 0xFFFF then 2 else 1
-- ANCHOR_END: positions

-- ANCHOR: uriPath
-- 🔗 file:///Users/me/My%20shop/Shop.lam → /Users/me/My shop/Shop.lam
--    (%20 is a space: a URI writes some characters as bytes).
-- %20 は空白。URI は、いくつかの文字をバイトで書く
uriPath :: Text -> FilePath
uriPath uri = T.unpack (TE.decodeUtf8Lenient (B.pack (bytes path)))
  where
    path = T.unpack (fromMaybe uri (T.stripPrefix "file://" uri))
    bytes s = case s of
      '%' : a : b : rest | isHexDigit a, isHexDigit b ->
        fromIntegral (digitToInt a * 16 + digitToInt b) : bytes rest
      ch : rest -> B.unpack (TE.encodeUtf8 (T.singleton ch)) <> bytes rest
      [] -> []
-- ANCHOR_END: uriPath

-- ANCHOR: json
-- 🔎 What is at a path of keys in a JSON value.
-- JSON の値の中の、キーの道の先にあるもの
at :: [Text] -> Value -> Maybe Value
at keys v = case (keys, v) of
  ([], _)             -> Just v
  (k : more, Object o) -> KeyMap.lookup (Key.fromText k) o >>= at more
  _                   -> Nothing

textAt :: [Text] -> Value -> Maybe Text
textAt keys v = case at keys v of
  Just (String t) -> Just t
  _               -> Nothing

intAt :: [Text] -> Value -> Maybe Int
intAt keys v = case at keys v of
  Just (Number n) -> toBoundedInteger n
  _               -> Nothing

-- 📝 The whole text of a didChange: the last change (we asked for the
--    whole text each time).
-- didChange の文字全体：最後の変更（毎回まるごと、と頼んである）
lastChange :: Value -> Maybe Text
lastChange params = case at ["contentChanges"] params of
  Just (Array cs) | not (null cs) -> textAt ["text"] (last (foldr (:) [] cs))
  _                               -> Nothing

-- ✉️ The answer to a request, and the answer that refuses it.
-- 頼みへの答えと、断る答え
reply :: Value -> Value -> Value
reply i result =
  object [ "jsonrpc" .= ("2.0" :: Text), "id" .= i, "result" .= result ]

refuse :: Value -> Text -> Value
refuse i method = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id"      .= i
  , "error"   .= object [ "code"    .= (-32601 :: Int)
                        , "message" .= ("unknown method: " <> method) ] ]
-- ANCHOR_END: json

-- ANCHOR: framing
-- 📥 Read one message: the headers (Content-Length: 52), an empty
--    line, then that many bytes of JSON. Nothing at the end of input.
-- 知らせを1つ読む：ヘッダー（Content-Length: 52）、空の行、それから
-- その数のバイトの JSON。入力の終わりなら Nothing
readMessage :: Handle -> IO (Maybe B.ByteString)
readMessage h = headers Nothing
  where
    headers size = do
      end <- hIsEOF h
      if end then pure Nothing else do
        line <- B8.filter (/= '\r') <$> B8.hGetLine h
        case (B8.null line, size) of
          (True, Just n)  -> Just <$> B.hGet h n
          (True, Nothing) -> headers Nothing
          (False, _)      -> headers (lengthOf line `orElse` size)
    lengthOf line = do
      rest <- B8.stripPrefix "Content-Length:" line
      fst <$> B8.readInt (B8.dropWhile (== ' ') rest)
    orElse a b = maybe b Just a

-- 📤 Write one message, with its header.
-- 知らせを1つ、ヘッダーを付けて書く
writeMessage :: Handle -> Value -> IO ()
writeMessage h v = do
  let body = BL.toStrict (encode v)
  B.hPut h ("Content-Length: " <> B8.pack (show (B.length body))
              <> "\r\n\r\n" <> body)
  hFlush h
-- ANCHOR_END: framing

-- ANCHOR: serve
-- 🔁 The loop: read a message, answer it, until exit (or the end of
--    the input). A message that is not JSON is skipped.
-- ループ：知らせを読み、答える。exit（か、入力の終わり）まで。JSON で
-- ない知らせは飛ばす
serve :: Handle -> Handle -> Files IO -> Session -> Server -> IO ()
serve input output files base start = do
  hSetBinaryMode input True
  hSetBinaryMode output True
  go start
  where
    go server = do
      got <- readMessage input
      forM_ got $ \bytes -> case eitherDecodeStrict bytes of
        Left _ -> go server
        Right msg -> do
          (server', out) <- step files base server msg
          mapM_ (writeMessage output) out
          unless (finished server') (go server')
-- ANCHOR_END: serve
-- ANCHOR_END: all
