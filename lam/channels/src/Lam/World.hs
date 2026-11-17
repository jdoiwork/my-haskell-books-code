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
  , cleanPath
  , fakeClock
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (IOException, bracket, try)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (getCurrentTime, getCurrentTimeZone, timeZoneMinutes)
import System.Directory (copyFile, createDirectoryIfMissing,
                         doesDirectoryExist, doesPathExist, getFileSize,
                         getModificationTime, listDirectory, removeDirectory,
                         removeFile, renamePath)
import System.Exit (ExitCode (..))
import qualified System.FilePath.Glob as G
import System.FilePath (normalise, splitDirectories, joinPath)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Random (StdGen, initStdGen, mkStdGen, uniformR)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (State, StateT, gets, modify',
                                   runState, runStateT, state)
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
import Lam.Io (Concurrent (..))
import Lam.Scheduler (Clockwork (..), schedule)
import Lam.Files (fileInfoValue, millisOf, outcomeValue, timeValue, zoneValue)
import Lam.Module (Files (..))
import Lam.Outside (responseValue)
import Lam.Sqlite
-- ANCHOR_END: head

-- ANCHOR: real
-- 🌍 The real world: here, at last, Haskell's IO does the work.
-- 本物の世界：ここで、やっと Haskell の IO が仕事をする
realWorld :: Outside -> Request -> IO Value
realWorld outside request = case request of
  -- ANCHOR: realScreen
  -- One writer at a time: jobs at once would mix their letters.
  -- 書くのは1人ずつ：同時に動く仕事が書くと、文字が混ざる
  PutStr s -> withMVar (outsideScreen outside) $ \_ -> do
    TIO.putStr s
    hFlush stdout                -- show "name? " before waiting
    pure unit
  -- ANCHOR_END: realScreen
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
  PutErr s -> withMVar (outsideScreen outside) $ \_ -> do
    TIO.hPutStr stderr s
    pure unit
  -- ANCHOR_END: realStdio
  -- ANCHOR: realFilesReq
  -- Files and folders, with Haskell's directory. Each failure is an Err.
  -- ファイルとフォルダー。Haskell の directory で。失敗はどれも Err
  ListDir dir -> do
    result <- try (listDirectory (T.unpack dir))
    pure (resultOf dir (strings . sort <$> result))
  Glob pattern -> do
    result <- try (G.glob (T.unpack pattern))
    pure (resultOf pattern (strings . sort . map normalise <$> result))
  Exists path -> VBool <$> doesPathExist (T.unpack path)
  IsDir path  -> VBool <$> doesDirectoryExist (T.unpack path)
  FileInfo path -> do
    result <- try $ do
      size <- getFileSize (T.unpack path)
      time <- getModificationTime (T.unpack path)
      pure (fileInfoValue size (millisOf time))
    pure (resultOf path result)
  MakeDir dir -> done dir (createDirectoryIfMissing True (T.unpack dir))
  Remove path -> done path $ do
    folder <- doesDirectoryExist (T.unpack path)
    (if folder then removeDirectory else removeFile) (T.unpack path)
  Rename from to -> done from (renamePath (T.unpack from) (T.unpack to))
  CopyFile from to -> done from (copyFile (T.unpack from) (T.unpack to))
  AppendFile path text -> done path (Utf8.appendFile (T.unpack path) text)
  -- ANCHOR_END: realFilesReq
  -- ANCHOR: realProcess
  -- A command, with no shell between. A command that is not there
  -- cannot start; one that ends with 1 is still an answer.
  -- シェルを通さずにコマンドを動かす。ないコマンドは始まれない。
  -- 1 で終わったコマンドは、それでも答え
  RunProcess command args input -> do
    result <- try (readCreateProcessWithExitCode
                     (proc (T.unpack command) (map T.unpack args))
                     (T.unpack input))
    pure $ outcomeValue $ case result of
      Left e -> Left (command <> ": " <> whyNot e)
      Right (code, out, err) ->
        Right (codeOf code, T.pack out, T.pack err)
  -- ANCHOR_END: realProcess
  -- ANCHOR: realClock
  Now -> timeValue . millisOf <$> getCurrentTime
  Here -> zoneValue . timeZoneMinutes <$> getCurrentTimeZone
  RandomInt lo hi -> atomicModifyIORef' (outsideRandom outside) $ \g ->
    let (n, g') = uniformR (lo, hi) g in (g', VInt n)
  -- ANCHOR_END: realClock
  -- ANCHOR: realSleep
  -- threadDelay waits in microseconds; only this thread waits.
  -- threadDelay はマイクロ秒で待つ。待つのは、このスレッドだけ
  Sleep ms -> VTuple [] <$ threadDelay (fromInteger ms * 1000)
  Uptime -> do
    now <- getMonotonicTimeNSec
    pure (VInt (toInteger ((now - outsideStart outside) `div` 1000000)))
  -- ANCHOR_END: realSleep
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
  { outsideManager   :: MVar (Maybe Manager)
  , outsideDatabases :: Databases
  -- ANCHOR: outsideRandom
  -- The dice: from --seed=618 if it is given, or else from the system.
  -- さいころ：--seed=618 があればそこから、なければシステムから
  , outsideRandom    :: IORef StdGen
  -- ANCHOR_END: outsideRandom
  -- When the program started (a clock that never goes back), for
  -- the times of --trace.
  -- プログラムが始まった時刻（戻らない時計）。--trace の時間のため
  , outsideStart     :: Word64
  -- ANCHOR: outsideScreen
  -- The key of the screen: hold it while you write.
  -- 画面の鍵：書くあいだ握っておく
  , outsideScreen    :: MVar ()
  -- ANCHOR_END: outsideScreen
  }

newOutside :: Maybe Int -> IO Outside
newOutside seed = do
  gen <- maybe initStdGen (pure . mkStdGen) seed
  Outside <$> newMVar Nothing <*> realDatabases <*> newIORef gen
          <*> getMonotonicTimeNSec <*> newMVar ()

-- 🔒 At the end: close the databases a script forgot to close.
-- 終わりに：スクリプトが閉じ忘れたデータベースを閉じる
closeOutside :: Outside -> IO ()
closeOutside = closeDatabases . outsideDatabases

withOutside :: Maybe Int -> (Outside -> IO a) -> IO a
withOutside seed = bracket (newOutside seed) closeOutside
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
    -- ANCHOR: realResponse
    Right response -> con "Ok" [responseValue
      (statusCode (H.responseStatus response))
      (Map.fromList
          [ (T.toLower (decode (CI.original k)), VString (decode v))
          | (k, v) <- H.responseHeaders response ])
      (decode (BL.toStrict (H.responseBody response)))]
    -- ANCHOR_END: realResponse
  where
    decode = TE.decodeUtf8With TE.lenientDecode
    -- 「つながらない」「時間内に答えがない」「URL ではない」
    reasonOf e = case e of
      HttpExceptionRequest _ (ConnectionFailure _) -> "cannot connect"
      HttpExceptionRequest _ ResponseTimeout       -> "no answer in time"
      HttpExceptionRequest _ other                 -> T.pack (show other)
      InvalidUrlException _ why  -> T.pack ("not a URL: " <> why)

-- ANCHOR: managerOf
-- 🔑 The manager, made at the first request. modifyMVar holds the box
--    while it looks and makes, so two jobs at once make only one.
-- マネージャーを、最初の頼みごとのときに作る。modifyMVar は、見て作る
-- あいだ箱を握っているので、同時に2つの仕事が来ても1つしか作らない
managerOf :: Outside -> IO Manager
managerOf outside = modifyMVar (outsideManager outside) $ \known ->
  case known of
    Just m  -> pure (known, m)
    Nothing -> do
      m <- newTlsManager
      pure (Just m, m)
-- ANCHOR_END: managerOf
-- ANCHOR_END: httpSend

-- ✅ Ok () when the action did it, or an Err.
-- やれたら Ok ()、だめなら Err
done :: Text -> IO () -> IO Value
done path act = do
  result <- try act
  pure (resultOf path (unit <$ result))

strings :: [FilePath] -> Value
strings = VList . map (VString . T.pack)

codeOf :: ExitCode -> Int
codeOf code = case code of
  ExitSuccess   -> 0
  ExitFailure n -> n

-- 「そういうコマンドはない」
whyNot :: IOException -> Text
whyNot e
  | isDoesNotExistError e = "no such command"
  | isPermissionError e   = "permission denied"
  | otherwise             = T.pack (show e)

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
  -- ANCHOR: fakeFilesFields
  -- Folders (the ones above a file are there, too), the time each file
  -- was changed, the commands that answer, a clock that never moves,
  -- the zone, and the dice (always the same seed).
  -- フォルダー（ファイルの上のフォルダーもある）、ファイルを変えた時刻、
  -- 答えるコマンド、止まった時計、ゾーン、さいころ（いつも同じ種）
  , worldDirs     :: Set Text
  , worldTimes    :: Map Text Integer             -- ms since 1970
  , worldCommands :: Map [Text] (Int, Text, Text) -- code, out, err
  , worldClock    :: Integer                      -- ms since 1970
  , worldZone     :: Int                          -- minutes from UTC
  , worldRandom   :: StdGen
  -- ANCHOR_END: fakeFilesFields
  -- ANCHOR: fakeTimeFields
  -- When the program started (for --trace), and how long each URL
  -- takes to answer (0 if not here). sleep and these move the clock.
  -- プログラムが始まった時刻（--trace のため）と、URL ごとの答えるまでの
  -- 時間（なければ 0）。sleep とこれが、時計を進める
  , worldStart    :: Integer                      -- ms since 1970
  , worldDelays   :: Map Text Integer             -- URL, ms
  -- ANCHOR_END: fakeTimeFields
  -- ANCHOR: fakeChanFields
  -- How many channels were made (for their numbers), and a seed to
  -- mix the jobs whose clocks are the same (Nothing: in order).
  -- 作ったチャネルの数（番号のため）と、時計が同じ仕事を混ぜる種
  -- （Nothing なら順に）
  , worldChans    :: Int
  , worldShuffle  :: Maybe StdGen
  -- ANCHOR_END: fakeChanFields
  }
  deriving (Eq, Show)

emptyWorld :: FakeWorld
emptyWorld = FakeWorld
  { worldInput = [], worldFiles = Map.empty, worldOutput = []
  , worldArgs = [], worldEnv = Map.empty, worldErrors = []
  , worldWeb = Map.empty, worldSent = []
  , worldDirs = Set.empty, worldTimes = Map.empty
  , worldCommands = Map.empty, worldClock = fakeClock, worldZone = 540
  , worldRandom = mkStdGen 618
  , worldStart = fakeClock, worldDelays = Map.empty
  , worldChans = 0, worldShuffle = Nothing }

-- ⏰ The fake clock: 2026-11-17 12:00 in Tokyo (03:00 in UTC).
-- 偽物の時計：東京の 2026-11-17 12:00（UTC の 03:00）
fakeClock :: Integer
fakeClock = 1794884400000

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
    found <- gets (Map.lookup (cleanPath path) . worldFiles)
    pure $ case found of
      Just text -> con "Ok" [VString text]
      Nothing   -> con "Err" [VString (path <> ": no such file")]
  WriteFile path text -> do
    modify' (putFile (cleanPath path) text)
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
    -- The answer comes after the delay of the URL.
    -- 答えは、その URL の待ち時間のあとに来る
    delay <- gets (Map.findWithDefault 0 url . worldDelays)
    tick delay
    found <- gets (Map.lookup url . worldWeb)
    pure $ case found of
      Just (status, text) ->
        con "Ok" [responseValue status Map.empty text]
      Nothing -> con "Err" [VString (url <> ": cannot connect")]
  -- ANCHOR_END: fakeWeb
  -- ANCHOR: fakeFilesReq
  ListDir dir -> withWorld $ \w -> case childrenOf w (cleanPath dir) of
    Just names -> ok (VList (map VString names))
    Nothing    -> bad dir "no such folder"
  Glob pattern -> withWorld $ \w ->
    let wanted = G.compile (T.unpack (cleanPath pattern))
    in ok (VList [ VString p | p <- Set.toAscList (allPaths w)
                             , G.match wanted (T.unpack p) ])
  Exists path -> withWorld $ \w ->
    VBool (isFile w (cleanPath path) || isFolder w (cleanPath path))
  IsDir path -> withWorld $ \w -> VBool (isFolder w (cleanPath path))
  FileInfo path -> withWorld $ \w ->
    let p = cleanPath path
    in case Map.lookup p (worldFiles w) of
         Just text -> ok (fileInfoValue (bytes text) (timeOf w p))
         Nothing | isFolder w p -> ok (fileInfoValue 0 (worldClock w))
                 | otherwise    -> bad path "no such file"
  MakeDir dir -> change $ \w ->
    let d = cleanPath dir
    in if isFile w d then Left (bad dir "a file is there")
       else Right w { worldDirs = Set.insert d (worldDirs w) }
  Remove path -> change $ \w ->
    let p = cleanPath path
    in if isFile w p then
         Right (keepParents p w { worldFiles = Map.delete p (worldFiles w)
                                , worldTimes = Map.delete p (worldTimes w) })
       else case childrenOf w p of
         Just [] -> Right w { worldDirs = Set.delete p (worldDirs w) }
         Just _  -> Left (bad path "the folder is not empty")
         Nothing -> Left (bad path "no such file")
  Rename from to -> change $ \w ->
    let (f, t) = (cleanPath from, cleanPath to)
        moved k = if k == f then t
                  else maybe k (t <>) (T.stripPrefix f k >>= slashed)
        slashed rest = if "/" `T.isPrefixOf` rest then Just rest else Nothing
        kept = keepParents f w
    in if isFile w f || isFolder w f
         then Right kept
                { worldFiles = Map.mapKeys moved (worldFiles kept)
                , worldTimes = Map.mapKeys moved (worldTimes kept)
                , worldDirs  = Set.map moved (worldDirs kept) }
         else Left (bad from "no such file")
  CopyFile from to -> change $ \w ->
    case Map.lookup (cleanPath from) (worldFiles w) of
      Just text -> Right (putFile (cleanPath to) text w)
      Nothing   -> Left (bad from "no such file")
  AppendFile path text -> change $ \w ->
    let p = cleanPath path
    in Right (putFile p (Map.findWithDefault "" p (worldFiles w) <> text) w)
  -- ANCHOR_END: fakeFilesReq
  -- ANCHOR: fakeProcess
  -- A command answers only if the world knows it: no shell, no disk.
  -- コマンドは、世界が知っているものだけが答える。シェルもディスクもない
  RunProcess command args _ -> withWorld $ \w ->
    outcomeValue $ case Map.lookup (command : args) (worldCommands w) of
      Just answer -> Right answer
      Nothing     -> Left (command <> ": no such command")
  -- ANCHOR_END: fakeProcess
  -- ANCHOR: fakeClock
  -- The clock never moves, and the dice start from the same seed: the
  -- same program gives the same answer, every time.
  -- 時計は動かず、さいころはいつも同じ種から。同じプログラムは、
  -- いつも同じ答えを出す
  Now  -> withWorld (timeValue . worldClock)
  -- ANCHOR: fakeSleep
  -- sleep only moves the fake clock: a test never waits.
  -- sleep は偽の時計を進めるだけ。テストは待たない
  Sleep ms -> VTuple [] <$ tick ms
  Uptime -> withWorld (\w -> VInt (worldClock w - worldStart w))
  -- ANCHOR_END: fakeSleep
  Here -> withWorld (zoneValue . worldZone)
  RandomInt lo hi -> do
    (n, g) <- gets (uniformR (lo, hi) . worldRandom)
    modify' (\w -> w { worldRandom = g })
    pure (VInt n)
  -- ANCHOR_END: fakeClock
  -- A pure world has no databases: see fakeWorldIO.
  -- 純粋な世界には、データベースがない。fakeWorldIO を見る
  SqlOpen file _ -> pure (con "Err" [VString (file <> ": no databases here")])
  SqlQuery {}    -> pure (con "Err" [VString "Sql: no databases here"])
  SqlExec {}     -> pure (con "Err" [VString "Sql: no databases here"])
  SqlClose _     -> pure unit
  where
    sent m u b = m <> " " <> u <> (if T.null b then "" else " " <> b)
    withWorld f = gets f
    ok v = con "Ok" [v]
    bad path why = con "Err" [VString (path <> ": " <> why)]
    -- Change the world, or answer an Err and leave it as it was.
    -- 世界を変えるか、Err を答えて、そのままにする
    change f = do
      w <- gets id
      case f w of
        Left err -> pure err
        Right w' -> do
          modify' (const w')
          pure (ok unit)
    bytes = toInteger . BL.length . BL.fromStrict . TE.encodeUtf8
    timeOf w p = Map.findWithDefault (worldClock w) p (worldTimes w)

-- ANCHOR: fakeFolders
-- 🧹 "./logs/../logs/a.log" is "logs/a.log", and "." is ".". (The
--    fake world has no links, so ".." can simply go up.)
-- 道を、くらべられる形にそろえる（偽物の世界にリンクはないので、".." は
-- そのまま1つ上へ）
cleanPath :: Text -> Text
cleanPath path =
  case reverse (foldl up [] (splitDirectories (normalise (T.unpack path)))) of
    []     -> "."
    pieces -> T.pack (joinPath pieces)
  where
    up acc piece = case (piece, acc) of
      (".", _)           -> acc
      ("..", p : rest) | p /= ".." -> rest
      _                  -> piece : acc

-- 📁 The folders above a path stay, even when the path goes away.
-- 道がなくなっても、その上のフォルダーは残る
keepParents :: Text -> FakeWorld -> FakeWorld
keepParents p w = w { worldDirs = worldDirs w <> Set.fromList
  [ T.intercalate "/" (take n pieces) | n <- [1 .. length pieces - 1] ] }
  where
    pieces = T.splitOn "/" p

-- ✍️ Put a file, changed now.
-- ファイルを置く。変えた時刻は、いま
putFile :: Text -> Text -> FakeWorld -> FakeWorld
putFile p text w = w
  { worldFiles = Map.insert p text (worldFiles w)
  , worldTimes = Map.insert p (worldClock w) (worldTimes w) }

isFile :: FakeWorld -> Text -> Bool
isFile w p = Map.member p (worldFiles w)

-- 📁 A folder is there if it was made, or if something is inside it.
-- フォルダーがあるのは、作ったときか、中に何かがあるとき
isFolder :: FakeWorld -> Text -> Bool
isFolder w p = p == "." || Set.member p (worldDirs w) || not (null inside)
  where
    inside = [ k | k <- Set.toList (allPaths w), (p <> "/") `T.isPrefixOf` k ]

-- 📁 The names just inside a folder, in order.
-- フォルダーのすぐ中の名前を、順に
childrenOf :: FakeWorld -> Text -> Maybe [Text]
childrenOf w p
  | isFolder w p = Just (Set.toAscList (Set.fromList
      [ T.takeWhile (/= '/') rest
      | k <- Set.toList (allPaths w)
      , Just rest <- [if p == "." then Just k
                      else T.stripPrefix (p <> "/") k] ]))
  | otherwise = Nothing

-- 🌳 Every file and every folder of the world (the ones above a file
--    too), as paths.
-- 世界のファイルとフォルダー全部（ファイルの上のフォルダーも）
allPaths :: FakeWorld -> Set Text
allPaths w = Set.fromList (concatMap withParents
  (Map.keys (worldFiles w) <> Set.toList (worldDirs w)))
  where
    withParents k =
      let pieces = T.splitOn "/" k
      in [ T.intercalate "/" (take n pieces) | n <- [1 .. length pieces] ]
-- ANCHOR_END: fakeFolders

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

-- ⏩ Move the fake clock.
-- 偽の時計を進める
tick :: Monad m => Integer -> StateT FakeWorld m ()
tick ms = modify' (\w -> w { worldClock = worldClock w + ms })

-- ANCHOR: fakeConcurrent
-- 🎭 A fake world runs the jobs on its fake clock, taking turns
--    (Lam.Scheduler). The same program gives the same turns and the
--    same output, every run. With worldShuffle (a seed), the jobs whose
--    clocks are the same take turns in a mixed order: another seed,
--    another order, to find a bug that hides in one order.
-- 偽物の世界は、偽の時計の上で、順番に仕事を動かす（Lam.Scheduler）。
-- 同じプログラムなら、いつも同じ順番、同じ出力。worldShuffle（種）が
-- あれば、時計が同じ仕事の順番を混ぜる。種が変われば順番も変わり、
-- ある順番のときだけ出るバグを探せる
instance Monad m => Concurrent (StateT FakeWorld m) where
  runAction = schedule Clockwork
    { clockNow  = gets worldClock
    , clockSet  = \t -> modify' (\w -> w { worldClock = t })
    , newChanId = do
        n <- gets ((+ 1) . worldChans)
        n <$ modify' (\w -> w { worldChans = n })
    , tieBreak  = do
        mixer <- gets worldShuffle
        case mixer of
          Nothing -> pure Nothing
          Just g  -> do
            let (r, g') = uniformR (0, 1000000) g
            Just r <$ modify' (\w -> w { worldShuffle = Just g' })
    }
-- ANCHOR_END: fakeConcurrent

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
