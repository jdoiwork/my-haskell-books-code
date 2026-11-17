-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Two worlds for the runner: the real one (the screen, the keyboard
--   and the disk), and a fake one for the tests (lines and files kept
--   in a Haskell value).
-- 実行役の2つの世界：本物（画面、キーボード、ディスク）と、
-- テスト用の偽物（行とファイルを、Haskell の値に持つ）。
module Lam.World
  ( realWorld
  , Outside
  , newOutside
  , closeOutside
  , withOutside
  , FakeWorld (..)
  , emptyWorld
  , fakeWorld
  , fakeWorldIO
  , runFake
  , runFakeIO
  , outputOf
  , errorsOf
  , realFiles
  , fakeFiles
  ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (State, StateT, gets, modify', runState,
                                   runStateT, state)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..),
                            Manager, RequestBody (..), httpLbs, parseRequest)
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.FilePath (splitSearchPath)
import System.IO (hFlush, isEOF, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)

import Lam.Builtins (con, maybeValue)
import Lam.Eval
import Lam.Module (Files (..))
import Lam.Sqlite
-- ANCHOR_END: head

-- ANCHOR: real
-- 🌍 The real world: here, at last, Haskell's IO does the work.
-- 本物の世界：ここで、やっと Haskell の IO が仕事をする
realWorld :: Outside -> Request -> IO Value
realWorld outside request = case request of
  PutStr s -> do
    TIO.putStr s
    hFlush stdout                -- show "name? " before waiting
    pure unit
  GetLine -> do
    end <- isEOF
    if end
      then pure (con "Nothing" [])
      else do
        line <- TIO.getLine
        pure (con "Just" [VString line])
  ReadFile path -> do
    result <- try (Utf8.readFile (T.unpack path))
    pure (resultOf path (VString <$> result))
  WriteFile path text -> do
    result <- try (Utf8.writeFile (T.unpack path) text)
    pure (resultOf path (unit <$ result))
  -- ANCHOR: realArgs
  -- The arguments after the file name: Main puts them with withArgs.
  -- ファイルの名前より後ろの引数。Main が withArgs で置いておく
  GetArgs -> do
    args <- getArgs
    pure (VList (map (VString . T.pack) args))
  LookupEnv key -> do
    found <- lookupEnv (T.unpack key)
    pure (maybeValue (VString . T.pack <$> found))
  GetEnvironment -> do
    pairs <- getEnvironment
    pure (VMap (Map.fromList
      [ (T.pack k, VString (T.pack v)) | (k, v) <- pairs ]))
  -- ANCHOR_END: realArgs
  -- ANCHOR: realStdio
  GetContents -> VString <$> Utf8.getContents
  PutErr s -> do
    TIO.hPutStr stderr s
    pure unit
  -- ANCHOR_END: realStdio
  -- ANCHOR: realOutside
  -- HTTP with http-client; SQLite with the table of Lam.Sqlite.
  -- HTTP は http-client で。SQLite は Lam.Sqlite の表で
  HttpSend method url headers body -> httpSend outside method url headers body
  _ -> case sqlRequest (outsideDatabases outside) request of
    Just act -> act
    Nothing  -> error "realWorld: a request with no handler"
  -- ANCHOR_END: realOutside

-- ANCHOR: outside
-- 🌐 What the real world keeps while a program runs: the HTTP manager
--    (made at the first request, so a script without HTTP does not
--    wait for it), and the open databases.
-- 本物の世界が、プログラムを動かすあいだ持つもの：HTTP のマネージャー
-- （最初の頼みごとのときに作る。HTTP を使わないスクリプトを待たせない）
-- と、開いているデータベース
data Outside = Outside
  { outsideManager   :: IORef (Maybe Manager)
  , outsideDatabases :: Databases
  }

newOutside :: IO Outside
newOutside = Outside <$> newIORef Nothing <*> realDatabases

-- 🔒 At the end: close the databases a script forgot to close.
-- 終わりに：スクリプトが閉じ忘れたデータベースを閉じる
closeOutside :: Outside -> IO ()
closeOutside = closeDatabases . outsideDatabases

withOutside :: (Outside -> IO a) -> IO a
withOutside = bracket newOutside closeOutside
-- ANCHOR_END: outside

-- ANCHOR: httpSend
-- 🌐 One HTTP request. A status like 404 is still an answer (Ok); only
--    "no answer" is an Err.
-- HTTP の頼みごと1つ。404 のような状態も答え（Ok）。Err になるのは、
-- 答えが来なかったときだけ
httpSend :: Outside -> Text -> Text -> [(Text, Text)] -> Text -> IO Value
httpSend outside method url headers body = do
  manager <- managerOf outside
  result <- try $ do
    request <- parseRequest (T.unpack url)
    httpLbs request
      { H.method = TE.encodeUtf8 method
      , H.requestHeaders =
          ("User-Agent", "lam/0.15")
            : [ (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)
              | (k, v) <- headers ]
      , H.requestBody = RequestBodyBS (TE.encodeUtf8 body) }
      manager
  pure $ case result of
    Left e -> con "Err" [VString (url <> ": " <> reasonOf e)]
    Right response -> con "Ok" [con "Response"
      [ VInt (toInteger (statusCode (H.responseStatus response)))
      , VMap (Map.fromList
          [ (T.toLower (decode (CI.original k)), VString (decode v))
          | (k, v) <- H.responseHeaders response ])
      , VString (decode (BL.toStrict (H.responseBody response))) ]]
  where
    decode = TE.decodeUtf8With TE.lenientDecode
    -- 「つながらない」「時間内に答えがない」「URL ではない」
    reasonOf e = case e of
      HttpExceptionRequest _ (ConnectionFailure _) -> "cannot connect"
      HttpExceptionRequest _ ResponseTimeout       -> "no answer in time"
      HttpExceptionRequest _ other                 -> T.pack (show other)
      InvalidUrlException _ why  -> T.pack ("not a URL: " <> why)

managerOf :: Outside -> IO Manager
managerOf outside = do
  known <- readIORef (outsideManager outside)
  case known of
    Just m  -> pure m
    Nothing -> do
      m <- newTlsManager
      writeIORef (outsideManager outside) (Just m)
      pure m
-- ANCHOR_END: httpSend

-- 📦 Ok v, or Err with a short message for people.
-- Ok v か、人が読む短い文を入れた Err
resultOf :: Text -> Either IOException Value -> Value
resultOf path result = case result of
  Right v -> con "Ok" [v]
  Left e  -> con "Err" [VString (path <> ": " <> reason e)]
  where
    reason e
      | isDoesNotExistError e = "no such file"           -- 「ファイルがない」
      | isPermissionError e   = "permission denied"      -- 「許されていない」
      | otherwise             = T.pack (show e)

unit :: Value
unit = VTuple []
-- ANCHOR_END: real

-- ANCHOR: fake
-- 🎭 A fake world: the lines to type, the files, and the screen.
-- 偽物の世界：打ちこむ行、ファイル、画面
--    For scripts: the arguments, the environment, and the standard
--    error.
-- スクリプトのため：引数、環境変数、標準エラー
data FakeWorld = FakeWorld
  { worldInput  :: [Text]          -- the lines still to be read
  , worldFiles  :: Map Text Text   -- the name and the text of each file
  , worldOutput :: [Text]          -- what was written, the newest first
  -- ANCHOR: fakeScriptFields
  , worldArgs   :: [Text]          -- the arguments
  , worldEnv    :: Map Text Text   -- the environment variables
  , worldErrors :: [Text]          -- the standard error, the newest first
  -- ANCHOR_END: fakeScriptFields
  -- ANCHOR: fakeWebFields
  -- The web: the status and the body for each URL, and what was sent.
  -- Web：URL ごとの状態コードと本文、送ったもの
  , worldWeb    :: Map Text (Int, Text)
  , worldSent   :: [Text]          -- "GET https://...", the newest first
  -- ANCHOR_END: fakeWebFields
  }
  deriving (Eq, Show)

emptyWorld :: FakeWorld
emptyWorld = FakeWorld [] Map.empty [] [] Map.empty [] Map.empty []

-- 🎭 The fake handler: the same requests, the same answers, but every
--    one is only a change to a Haskell value. No disk, no keyboard.
-- 偽物のハンドラー：同じ頼みごとに、同じ形の答え。でも、どれも
-- Haskell の値を書き換えるだけ。ディスクにもキーボードにも触れない
fakeWorld :: Request -> State FakeWorld Value
fakeWorld request = case request of
  PutStr s -> do
    modify' (\w -> w { worldOutput = s : worldOutput w })
    pure unit
  GetLine -> do
    input <- gets worldInput
    case input of
      [] -> pure (con "Nothing" [])
      line : rest -> do
        modify' (\w -> w { worldInput = rest })
        pure (con "Just" [VString line])
  ReadFile path -> do
    found <- gets (Map.lookup path . worldFiles)
    pure $ case found of
      Just text -> con "Ok" [VString text]
      Nothing   -> con "Err" [VString (path <> ": no such file")]
  WriteFile path text -> do
    modify' (\w -> w { worldFiles = Map.insert path text (worldFiles w) })
    pure (con "Ok" [unit])
  -- ANCHOR: fakeArgs
  GetArgs -> do
    args <- gets worldArgs
    pure (VList (map VString args))
  LookupEnv key -> do
    found <- gets (Map.lookup key . worldEnv)
    pure (maybeValue (VString <$> found))
  GetEnvironment -> do
    env <- gets worldEnv
    pure (VMap (VString <$> env))
  -- ANCHOR_END: fakeArgs
  -- ANCHOR: fakeStdio
  -- All the lines still to be read, each with its line break.
  -- まだ読んでいない行を全部。どの行にも改行を付ける
  GetContents -> do
    input <- gets worldInput
    modify' (\w -> w { worldInput = [] })
    pure (VString (T.concat [ line <> "\n" | line <- input ]))
  PutErr s -> do
    modify' (\w -> w { worldErrors = s : worldErrors w })
    pure unit
  -- ANCHOR_END: fakeStdio
  -- ANCHOR: fakeWeb
  -- The fake web: the answer kept for the URL. Every request is noted.
  -- 偽物の Web：URL に決めておいた答え。どの頼みごとも記録する
  HttpSend method url _ body -> do
    modify' (\w -> w { worldSent = sent method url body : worldSent w })
    found <- gets (Map.lookup url . worldWeb)
    pure $ case found of
      Just (status, text) ->
        con "Ok" [con "Response" [VInt (toInteger status), VMap Map.empty,
                                  VString text]]
      Nothing -> con "Err" [VString (url <> ": cannot connect")]
  -- ANCHOR_END: fakeWeb
  -- A pure world has no databases: see fakeWorldIO.
  -- 純粋な世界には、データベースがない。fakeWorldIO を見る
  SqlOpen file _ -> pure (con "Err" [VString (file <> ": no databases here")])
  SqlQuery {}    -> pure (con "Err" [VString "Sql: no databases here"])
  SqlExec {}     -> pure (con "Err" [VString "Sql: no databases here"])
  SqlClose _     -> pure unit
  where
    sent m u b = m <> " " <> u <> (if T.null b then "" else " " <> b)

-- ANCHOR: fakeWorldIO
-- 🎭 The fake world with databases: SQLite in memory (memoryDatabases).
--    It touches neither the disk nor the network. The rest is the
--    same as fakeWorld.
-- データベースのある偽物の世界：メモリーの中の SQLite
-- （memoryDatabases）。ディスクにもネットにも触れない。ほかは fakeWorld
-- と同じ
fakeWorldIO :: Databases -> Request -> StateT FakeWorld IO Value
fakeWorldIO dbs request = case sqlRequest dbs request of
  Just act -> liftIO act
  Nothing  -> state (runState (fakeWorld request))

-- ▶️ Run something in the fake world with databases.
-- データベースのある偽物の世界で動かす
runFakeIO :: FakeWorld -> StateT FakeWorld IO a -> IO (a, FakeWorld)
runFakeIO world m = runStateT m world
-- ANCHOR_END: fakeWorldIO

-- ▶️ Run something in a fake world, and see the world afterwards.
-- 偽物の世界で動かし、そのあとの世界を見る
runFake :: FakeWorld -> State FakeWorld a -> (a, FakeWorld)
runFake world m = runState m world

-- 🖥️ Everything written to the screen, in order.
-- 画面に書いたもの全部を、順に
outputOf :: FakeWorld -> Text
outputOf = T.concat . reverse . worldOutput

-- 🖥️ Everything written to the standard error, in order.
-- 標準エラーに書いたもの全部を、順に
errorsOf :: FakeWorld -> Text
errorsOf = T.concat . reverse . worldErrors
-- ANCHOR_END: fake
-- ANCHOR: files
-- 📂 The files of modules in the real world: read from the disk.
--    LAM_PATH is a list of folders, joined with : (like PATH).
-- 本物の世界のモジュールのファイル：ディスクから読む。LAM_PATH は、
-- PATH と同じく : でつないだフォルダーの並び
realFiles :: IO (Files IO)
realFiles = do
  path <- lookupEnv "LAM_PATH"
  pure Files
    { readSource = \file -> do
        there <- doesFileExist file
        if there then Just <$> Utf8.readFile file else pure Nothing
    , searchPath = maybe [] splitSearchPath path }

-- 📂 The files of modules in a fake world: its worldFiles, with the
--    folders of LAM_PATH from its worldEnv.
-- 偽物の世界のモジュールのファイル：worldFiles から読む。LAM_PATH は
-- worldEnv から
fakeFiles :: Monad m => FakeWorld -> Files (StateT FakeWorld m)
fakeFiles world = Files
  { readSource = \file -> gets (Map.lookup (T.pack file) . worldFiles)
  , searchPath = maybe [] (splitSearchPath . T.unpack)
                   (Map.lookup "LAM_PATH" (worldEnv world)) }
-- ANCHOR_END: files
-- ANCHOR_END: all
