-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Aeson as A
import Data.Char (isAlpha, toLower)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import qualified Control.Exception as E
import System.Timeout (timeout)

import Data.Either (isLeft, isRight)

import Lam.Module
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, eof)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (modify, runState)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, replaceExtension, splitSearchPath,
                        takeDirectory, takeExtension, takeFileName, (</>))
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Random (mkStdGen)
import Lam.Format (groupDigits, padLeft, padRight, center, zeroPad)
import Lam.Builtins (preludeConstructors)
import Lam.Stdlib (splitOn)
import Lam.Json (fromAeson, toAeson)
import Lam.Parser hiding (parseExpr)
import qualified Lam.Parser as P
import Lam.Prelude
import Lam.Pretty
import Lam.Program
import Lam.Repl
import Lam.Sugar (builtinOperators)
import Lam.Syntax
import Lam.Types
import Lam.World
import Lam.Eval (Exchange (..), Cell (..), OpenMode (..), Request (..), Value (..),
                 ShellCommand (..), renderValue)
import Lam.Shell (displayWidth, quoteWord, stripStyles)
import Lam.Server (webRequest)
import Lam.Bundle
import Lam.Answers (groupNames, shadowWarnings)
import Lam.Sqlite
import Lam.Guard
import Lam.OneLiner
import Lam.Cli
import Lam.Utf8 (Decoding (..), decodeWith, hexOf)
import Lam.Pcre (Cut (..), Regex (..), compileRegex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Crypto.Hash.SHA256 as Sha
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile, openBinaryTempFile)
import System.Process (readProcessWithExitCode)
-- ANCHOR_END: head

-- 📋 The operators of the Prelude (every file starts with them).
-- プレリュードの演算子（どのファイルも、これから始まる）
ops :: Operators
ops = sessionOperators preludeSession

-- 📖 Read an expression with the operators of the Prelude.
-- プレリュードの演算子で、式を読む
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = P.parseExpr ops

-- ANCHOR: run
-- 🔁 Type the lines into a REPL with the prelude, one by one.
-- プレリュード入りの REPL に、1行ずつ打ちこむ
runLines :: [Text] -> [Text]
runLines inputs = snd (foldl go (preludeSession, []) inputs)
  where
    go (s, outs) line =
      let (s', out) = replStep s line in (s', outs <> [out])

-- 🔁 One turn of the REPL, in an empty fake world: what it shows.
-- REPL の1回分を、空の偽物の世界で。見せるもの
replStep :: Session -> Text -> (Session, Text)
replStep s line = fst (runFake emptyWorld (step fakeWorld s line))

-- 🧮 The expression at the end of a loaded program, done in an empty
--    fake world: what it shows.
-- 読みこんだプログラムの最後の式を、空の偽物の世界でやる。見せるもの
finalText :: (Session, Maybe Expr) -> Either Text (Session, Maybe Text)
finalText (s, final) = case final of
  Nothing -> Right (s, Nothing)
  Just e  -> (\t -> (s, Just t))
               <$> shownBy (fst (runFake emptyWorld (evaluate fakeWorld s e)))

-- 🏁 An ending, as the text it shows (or the error).
-- 終わり方を、見せる文字（かエラー）に
shownBy :: Ending -> Either Text Text
shownBy ending = case ending of
  Finished t   -> Right t
  Errored t    -> Left t
  ExitedWith n -> Left ("[exit " <> T.pack (show n) <> "]")

-- 🏁 An ending of a program: Right () when it went to the end.
-- プログラムの終わり方。最後まで行けば Right ()
endedBy :: Ending -> Either Text ()
endedBy = fmap (const ()) . shownBy

-- ANCHOR: runLinesIn
-- 🎭 Type the lines into a REPL in a fake world: what each one shows,
--    and the world afterwards.
-- 偽物の世界の REPL に、1行ずつ打ちこむ。それぞれが見せるものと、
-- そのあとの世界
runLinesIn :: FakeWorld -> [Text] -> ([Text], FakeWorld)
runLinesIn world inputs = runFake world (go preludeSession inputs)
  where
    go _ [] = pure []
    go s (line : rest) = do
      (s', out) <- step fakeWorld s line
      (out :) <$> go s' rest

-- 🎭 Load a program and run it in a fake world, as lam file.lam does.
-- プログラムを読みこみ、偽物の世界で、lam file.lam と同じように動かす
runFile
  :: Files Identity -> FakeWorld -> FilePath -> Text
  -> (Either Text (), FakeWorld)
runFile files world file source =
  let (ending, world') = runEnding files world file source
  in (endedBy ending, world')

-- 🎭 The same, with how it ended (exit n too).
-- 同じだが、終わり方（exit n も）を返す
runEnding
  :: Files Identity -> FakeWorld -> FilePath -> Text -> (Ending, FakeWorld)
runEnding files world file source = runFake world $
  case runIdentity (loadProgram files preludeSession preludeSession
                      file source) of
    Left err              -> pure (Errored err)
    Right (loaded, final) -> runProgram fakeWorld loaded final
-- ANCHOR_END: runLinesIn
-- ANCHOR_END: run

-- ANCHOR: genText
-- 🎲 Any text, made often of the letters that need care:
--    " \ new line, tab, and letters outside ASCII.
-- どんな文字列でも。ただし、気をつけたい文字を多めに混ぜる
genText :: Gen Text
genText = T.pack <$> listOf (frequency
  [ (3, elements "\"\\\n\t")
  , (3, elements "ab λラ")
  , (1, arbitrary) ])
-- ANCHOR_END: genText

-- ANCHOR: golden
-- 📜 Run an example program, as `lam file.lam` would show it, but in a
--    fake world: the lines of the .in file (if there is one) are typed
--    in, and no file is read or written on the disk. The modules are
--    every .lam file under examples/, and LAM_PATH is the line of the
--    .env file (if there is one).
-- 例のプログラムを、lam file.lam と同じように、でも偽物の世界で動かす。
-- .in ファイル（あれば）の行をキーボードから打ったことにし、ディスクの
-- ファイルは読みも書きもしない。モジュールは examples/ の下の .lam
-- ファイル全部。LAM_PATH は .env ファイルの行（あれば）
runExample :: FilePath -> IO Text
runExample file = do
  dbs <- memoryDatabases
  fst <$> runExampleWith dbs id file

-- ANCHOR: runExampleWith
-- 🎭 The same, with databases given (to run twice on the same ones),
--    and a change to the world before the run. The answer has the
--    world after. The .web file is the fake web (URL, status, file),
--    and the .flags file has the flags of lam, one in a line.
-- 同じだが、データベースを渡し（同じもので2回動かすため）、動かす前に
-- 世界を変えられる。答えには、あとの世界も入る。.web ファイルは偽物の
-- Web（URL・状態コード・ファイル）、.flags ファイルは lam のフラグ
-- （1行に1つ）
runExampleWith
  :: Databases -> (FakeWorld -> FakeWorld) -> FilePath
  -> IO (Text, FakeWorld)
runExampleWith dbs change file = do
  flags <- linesOf (dropExtension file <> ".flags")
  runFlagged dbs change (map T.unpack flags) file
  where
    linesOf f = do
      there <- doesFileExist f
      if there then T.lines <$> Utf8.readFile f else pure []
-- ANCHOR_END: runExampleWith

-- ANCHOR: runFlagged
-- 🚩 The same, with the flags of lam given (--allow-read, --dry-run,
--    ...): the guards are put around the fake world, as lam does it.
-- 同じだが、lam のフラグを渡す。lam と同じく、偽物の世界に見張りを
-- かぶせる
runFlagged
  :: Databases -> (FakeWorld -> FakeWorld) -> [String] -> FilePath
  -> IO (Text, FakeWorld)
runFlagged dbs change flags file = do
  (ending, done) <- runFlaggedEnding dbs change flags file
  -- The standard error comes after the screen, and then how it ended:
  -- an error, or [exit n] for an exit other than 0.
  -- 標準エラーは画面の後ろに。そのあとに終わり方：エラーか、0 のほかの
  -- exit なら [exit n]
  pure (outputOf done <> errorsOf done <> case ending of
          Finished _   -> ""
          Errored t    -> t <> "\n"
          ExitedWith 0 -> ""
          ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n"
          <> servedOf done, done)
-- ANCHOR_END: runFlagged

-- ANCHOR: servedOf
-- 🛎️ The answers a fake server gave, in the order they were given:
--    "<- GET /todos 200 [...]".
-- 偽物のサーバーが返した答えを、返した順に
servedOf :: FakeWorld -> Text
servedOf w = T.concat
  [ "<- " <> exMethod ex <> " " <> exPath ex <> " "
      <> T.pack (show (exStatus ex))
      <> firstLine (exBody ex)
      <> "\n"
  | ex <- reverse (worldServed w) ]
  where
    -- Only the first line of the body: "<!doctype html> …".
    -- 本文は1行目だけ
    firstLine body = case T.lines body of
      []        -> ""
      [one]     -> " " <> one
      one : _   -> " " <> one <> " …"
-- ANCHOR_END: servedOf

-- 🚩 The same, with how it ended (Errored, ExitedWith) as it is.
-- 同じだが、終わり方をそのまま返す
runFlaggedEnding
  :: Databases -> (FakeWorld -> FakeWorld) -> [String] -> FilePath
  -> IO (Ending, FakeWorld)
runFlaggedEnding dbs change flags file = do
  guard <- either (fail . T.unpack) (guarded . fst)
             (readOptions flags Nothing)
  source  <- Utf8.readFile file
  modules <- lamFiles "examples"
  env     <- linesOf (dropExtension file <> ".env")
  input   <- linesOf (dropExtension file <> ".in")
  shared  <- worldFilesOf (dropExtension file <> ".files")
  args    <- linesOf (dropExtension file <> ".args")
  web     <- linesOf (dropExtension file <> ".web") >>= mapM page
  -- ANCHOR: goldenFiles
  -- .times: a file (read from the disk) and when it was changed.
  -- .run: a command and its answer (the code, the file of its output).
  -- .times：ファイル（ディスクから読む）と、変えた時刻。
  -- .run：コマンドと、その答え（終了コード、出力のファイル）
  times   <- linesOf (dropExtension file <> ".times") >>= mapM stamp
  runs    <- linesOf (dropExtension file <> ".run") >>= mapM command
  -- .req: the requests from the web, "300 POST /todos {...}": when it
  -- comes (ms), the method, the path, and the body (the rest).
  -- .req：Web からの頼み。来る時刻（ミリ秒）、メソッド、道、本文（残り）
  reqs    <- linesOf (dropExtension file <> ".req") >>= mapM incoming
  -- ANCHOR_END: goldenFiles
  let path  = [ T.unpack (T.drop 1 v)
              | l <- env, let (k, v) = T.breakOn "=" l, k == "LAM_PATH" ]
      files = mapFiles (Map.fromList modules)
                (concatMap (splitSearchPath) path)
      world = emptyWorld { worldInput = input
                         , worldFiles = shared <> Map.fromList
                             [ (p, t) | (p, t, _) <- times ]
                         , worldTimes = Map.fromList
                             [ (p, ms) | (p, _, ms) <- times ]
                         , worldCommands = Map.fromList runs
                         , worldArgs = args
                         , worldEnv = Map.fromList
                             [("HOME", "/home/lam"), ("LANG", "C.UTF-8")]
                         , worldWeb = Map.fromList
                             [ (u, r) | (u, r, _) <- web ]
                         , worldDelays = Map.fromList
                             [ (u, d) | (u, _, d) <- web ]
                         , worldIncoming = reqs }
  -- The modules come from Identity; the program runs with databases.
  -- モジュールは Identity から。プログラムはデータベースつきで動かす
  (ending, done) <- runFakeIO (change world) $
    case runIdentity (loadProgram files preludeSession preludeSession
                        file source) of
      Left err              -> pure (Errored err)
      Right (loaded, final) ->
        runProgram (guard (fakeWorldIO dbs)) loaded final
  pure (ending, done)
  where
    linesOf f = do
      there <- doesFileExist f
      if there then T.lines <$> Utf8.readFile f else pure []
    -- The files of the fake world: each path in the .files list (from
    -- this project's folder), with what is in it on the disk.
    -- 偽物の世界のファイル：.files の一覧の道（このプロジェクトの
    -- フォルダーから）ごとに、ディスクの中身
    worldFilesOf f = do
      names <- linesOf f
      Map.fromList <$> mapM (\n -> (,) n <$> B.readFile (T.unpack n))
                            names
    -- "examples/files/logs/a.log 2026-11-01T09:10:00Z"
    stamp l = case T.words l of
      [p, iso] -> case (iso8601ParseM (T.unpack iso) :: Maybe UTCTime) of
        Just t -> do
          text <- B.readFile (T.unpack p)
          pure (p, text, floor (utcTimeToPOSIXSeconds t * 1000) :: Integer)
        Nothing -> fail ("a bad time in " <> file <> ": " <> T.unpack l)
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)
    -- "0 gitlog.txt git log --format=%an": the file is next to the .run.
    -- ファイルは .run ファイルの隣から
    command l = case T.words l of
      code : out : cmd -> do
        text <- Utf8.readFile (takeDirectory file </> T.unpack out)
        pure (cmd, (read (T.unpack code) :: Int, text, ""))
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)
    -- "300 POST /todos {"title": "..."}"
    incoming l = case T.words l of
      ms : method : target : _ ->
        let body = T.strip (T.drop (T.length (T.unwords [ms, method, target]))
                              (T.strip l))
        in pure (read (T.unpack ms) :: Integer, webRequest method target body)
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)
    -- "URL 200 web/tokyo.json": the file is next to the .web file.
    -- A 4th word: the milliseconds the answer takes (0 if none).
    -- ファイルは .web ファイルの隣から。
    -- 4つ目の語：答えるまでのミリ秒（なければ 0）
    page l = case T.words l of
      url : status : body : rest | length rest <= 1 -> do
        text <- Utf8.readFile (takeDirectory file </> T.unpack body)
        let delay = case rest of
              [ms] -> read (T.unpack ms) :: Integer
              _    -> 0
        pure (url, (read (T.unpack status) :: Int, text), delay)
      _ -> fail ("a bad line in " <> file <> ": " <> T.unpack l)

-- ANCHOR: scriptIO
-- 🎭 Run a script in the fake world with databases.
-- データベースのある偽物の世界で、スクリプトを動かす
scriptIO :: Databases -> FakeWorld -> Text -> IO (Ending, FakeWorld)
scriptIO dbs world source =
  runFakeIO world $
    case runIdentity (loadProgram (mapFiles Map.empty []) preludeSession
                        preludeSession "script.lam" source) of
      Left err              -> pure (Errored err)
      Right (loaded, final) -> runProgram (fakeWorldIO dbs) loaded final

-- ANCHOR: scriptFlagged
-- 🎭 Run a script in the fake world with databases, with the flags of
--    lam (the guards around the world).
-- データベースのある偽物の世界で、lam のフラグつきでスクリプトを動かす
scriptFlagged
  :: Databases -> [String] -> FakeWorld -> Text -> IO (Ending, FakeWorld)
scriptFlagged dbs flags world source = do
  guard <- either (fail . T.unpack) (guarded . fst)
             (readOptions flags Nothing)
  runFakeIO world $
    case runIdentity (loadProgram (mapFiles Map.empty []) preludeSession
                        preludeSession "script.lam" source) of
      Left err              -> pure (Errored err)
      Right (loaded, final) ->
        runProgram (guard (fakeWorldIO dbs)) loaded final

-- 🎲 Requests of every kind, for the property of the guards.
-- いろいろな頼みごと。見張りの性質のテストのため
sampleRequests :: [Request]
sampleRequests =
  [ PutStr "hi", GetLine, ReadFile "a.txt", WriteFile "a.txt" "x"
  , GetArgs, LookupEnv "HOME", GetEnvironment, GetContents
  , ListDir "logs", Glob "logs/*.log", Exists "a.txt", IsDir "logs"
  , FileInfo "a.txt", MakeDir "out", Remove "a.txt"
  , Rename "a.txt" "b.txt", CopyFile "a.txt" "b.txt"
  , AppendFile "a.txt" "x", RunProcess "git" ["log"] ""
  , Now, Here, RandomInt 1 6
  , OpenFile "big.log" Strict, ReadLine 1, ReadChunk 1 10, CloseFile 1
  , ReadBytes "a.png", WriteBytes "b.png" "x"
  , HttpSend "GET" "https://x.test/" [] ""
  , HttpSend "POST" "https://x.test/" [] "{}"
  , SqlOpen "a.db" ReadWrite, SqlOpen "a.db" ReadOnly
  , SqlOpen ":memory:" ReadWrite, SqlQuery 1 "SELECT 1" []
  , SqlExec 1 "DELETE FROM t" [], SqlClose 1
  , RunCommand (ShellCommand "git" ["status"] "" [] ""), IsTerminal
  , GetSecret ]
-- ANCHOR_END: scriptFlagged

-- 🎲 A cell, and how Lam shows it when it comes back. A decimal has
--    at most 9 digits, and does not end in 0 (17.40 comes back 17.4).
-- マスと、戻ってきたときの Lam の見え方。小数は9けたまでで、0 で
-- 終わらない（17.40 は 17.4 で戻る）
genCell :: Gen (Cell, Text)
genCell = oneof
  [ pure (CNull, "JNull")
  , (\i -> (CInt i, "JInt " <> paren i))
      <$> chooseInteger (-(2 ^ (63 :: Int)), 2 ^ (63 :: Int) - 1)
  , do d <- chooseInt (1, 4)
       c <- chooseInteger (-999999999, 999999999)
              `suchThat` (\c -> c `mod` 10 /= 0)
       pure (CDec c d, "JDec " <> paren c <> " " <> T.pack (show d))
  , (\t -> (CText t, "JStr " <> renderString t)) . T.pack
      <$> listOf arbitraryPrintableChar ]
  where
    paren n = if n < 0 then "(" <> T.pack (show n) <> ")" else T.pack (show n)
-- ANCHOR_END: scriptIO

-- ANCHOR: script
-- 🎭 Run a script in a fake world: how it ended, and the world after.
-- 偽物の世界でスクリプトを動かす：どう終わったかと、あとの世界
script :: FakeWorld -> Text -> (Ending, FakeWorld)
script world source =
  runEnding (mapFiles Map.empty []) world "script.lam" source
-- ANCHOR_END: script

-- ANCHOR: genWords
-- 🎲 Lines of words: a few words (so that some come again), with
--    capitals, signs and spaces between them.
-- 単語の行。単語は少しだけ（何度も出てくるように）。大文字・記号・
-- 空白を、あいだに混ぜる
genWords :: Gen [Text]
genWords = listOf line
  where
    line = T.concat <$> listOf piece
    piece = frequency
      [ (4, elements ["lam", "Lam", "haskell", "type", "ラムダ", "λ", "é"])
      , (3, elements [" ", ", ", ". ", "!", "  ", "1", "-"]) ]

-- 🏆 The top 10 in Haskell, as the first chapter counts them.
-- Haskell で数えたトップ10。最初の章と同じ数え方
haskellTop10 :: [Text] -> Text
haskellTop10 ls = T.concat
  [ w <> T.replicate (12 - T.length w) " " <> T.pack (show n) <> "\n"
  | (w, n) <- take 10 (sortOn (Down . snd) (Map.toList counts)) ]
  where
    text   = T.unlines ls
    ws     = T.words (T.map (\c -> if isAlpha c then toLower c else ' ') text)
    counts = Map.fromListWith (+) [ (w, 1 :: Int) | w <- ws ]
-- ANCHOR_END: genWords

-- 📂 Every .lam file under a folder: its path and its text.
-- フォルダーの下の .lam ファイル全部：道と中身
lamFiles :: FilePath -> IO [(FilePath, Text)]
lamFiles dir = do
  names <- listDirectory dir
  concat <$> mapM (\n -> do
    let path = dir </> n
    folder <- doesDirectoryExist path
    if folder then lamFiles path
    else if takeExtension n == ".lam"
      then (\t -> [(path, t)]) <$> Utf8.readFile path
      else pure []) names

-- 🗂️ Files from a Map (no disk), and the folders of LAM_PATH.
-- Map のファイル（ディスクなし）と、LAM_PATH のフォルダー
mapFiles :: Map.Map FilePath Text -> [FilePath] -> Files Identity
mapFiles table = Files (\f -> pure (Map.lookup f table))

-- 📜 Load a program that imports nothing of its own.
-- 自分の import がないプログラムを読みこむ
loadPlain :: Session -> FilePath -> Text -> Either Text (Session, Maybe Text)
loadPlain session file source =
  runIdentity (loadProgram noFiles preludeSession session file source)
    >>= finalText

-- 🗑️ The warning for a Result thrown away, after its place.
-- 捨てた Result への警告（場所のあと）
droppedText :: Text
droppedText = "warning: the Result of this line is thrown away: write ? \
  \at the end of the line to stop at an Err, or _ <- (or 🗑️ <-) to \
  \throw it away"

-- 🥇 Golden test: the output must be the same as the .out file.
-- ゴールデンテスト：出力が .out ファイルと同じであること
golden :: String -> Spec
golden base =
  it ("examples/" <> base <> ".lam") $ do
    actual   <- runExample ("examples/" <> base <> ".lam")
    expected <- Utf8.readFile ("examples/" <> base <> ".out")
    actual `shouldBe` expected
-- ANCHOR_END: golden

-- ANCHOR: genPair
-- 🎲 A key and a value. The keys are few, so the same key comes often.
-- キーと値。キーは少しだけなので、同じキーがよく出る
genPair :: Gen (Text, Integer)
genPair = (,) <$> elements ["a", "b", "λ", "\"q\""] <*> choose (0, 9)

-- 🖨️ Pairs, as Lam writes them: [("a", 1), ("b", 2)]
-- 組のリストを、Lam の書き方で
lamPairs :: [(Text, Integer)] -> Text
lamPairs kvs = "[" <> T.intercalate ", "
  [ "(" <> renderString k <> ", " <> T.pack (show v) <> ")" | (k, v) <- kvs ]
  <> "]"
-- ANCHOR_END: genPair

-- ANCHOR: genAeson
-- 🎲 Any JSON value of aeson, not too big. The numbers are made of
--    two whole numbers, as aeson keeps them: 1999 and -2 is 19.99.
-- aeson の JSON の値（大きすぎないもの）。数は、aeson と同じく、
-- 2つの整数から作る
genAeson :: Gen A.Value
genAeson = sized go
  where
    go n = oneof $
      [ pure A.Null
      , A.Bool <$> arbitrary
      , A.Number <$> (scientific <$> arbitrary <*> choose (-30, 30))
      , A.String <$> genText ]
      <> if n <= 0 then [] else
           [ A.Array . V.fromList <$> few (go (n `div` 3))
           , A.object <$> few
               ((\k v -> (Key.fromText k, v))
                  <$> genText <*> go (n `div` 3)) ]
    -- 0 to 4 of them, so that the value does not grow too big.
    -- 0〜4個。値が大きくなりすぎないように
    few g = do
      k <- choose (0, 4)
      vectorOf k g
-- ANCHOR_END: genAeson

main :: IO ()
main = hspec $ do
  -- 「糖衣構文」
  describe "sugar" $
    -- ANCHOR: sugar
    -- 「引数がいくつもあっても、木は1引数の関数の重なり」
    it "turns many arguments into one-argument functions" $
      map (fmap render . parseExpr)
        [ "\\x y -> x + y"
        , "let add x y = x + y in add 1 2" ]
        `shouldBe` map Right
          [ "\\x -> \\y -> x + y"
          , "let add = \\x -> \\y -> x + y in add 1 2" ]
    -- ANCHOR_END: sugar

  -- 「プレリュード」
  describe "prelude" $
    -- ANCHOR: prelude
    -- 「プレリュードの関数に、型が付いている」
    it "gives the prelude functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["id", "const", "compose", "flip", "not", "max", "gcd"]
        `shouldBe` map Just
          [ "a -> a", "a -> b -> a", "(a -> b) -> (c -> a) -> c -> b"
          , "(a -> b -> c) -> b -> a -> c", "Bool -> Bool"
          , "Comparable a => a -> a -> a", "Int -> Int -> Int" ]
    -- ANCHOR_END: prelude

  -- 「REPL」
  describe "step" $
    it "uses the prelude, and says which definition went wrong" $ do
      let inputs =
            [ "gcd 84 36", "compose not even 7"
            , "let add x y = x + y", "add 1 2"
            , "let bad = not 1", ":type twice twice" ]
          go (s, outs) line =
            let (s', out) = replStep s line in (s', outs <> [out])
      snd (foldl go (preludeSession, []) inputs)
        `shouldBe`
          [ "12 : Int", "true : Bool"
          , "add : Number a => a -> a -> a", "3 : Int"
          , "in the definition of bad: type error: \
            \the types do not fit: a number where Bool is needed"
          , "(a -> a) -> a -> a" ]

  -- 「文字列」
  describe "strings" $ do
    -- ANCHOR: stringRoundTrip
    -- 「どんな文字列でも、ソースコードに書いて読み直すと、同じ文字列」
    it "reads back any string it writes" $
      property $ forAll genText $ \s ->
        parseExpr (render (Lit (LString s))) == Right (Lit (LString s))
    -- ANCHOR_END: stringRoundTrip

    -- ANCHOR: stringExamples
    -- 「文字列をつなぎ、組み込みの関数で変える」
    it "joins strings and changes them with builtins" $
      runLines
        [ "\"Hello, \" ++ \"Lam\""
        , "toUpper \"lam\""
        , "length \"ラムダ\""
        , "\"say \\\"hi\\\"\\n\""
        , "\"a\" ++ 1" ]
        `shouldBe`
          [ "\"Hello, Lam\" : String"
          , "\"LAM\" : String"
          , "3 : Int"
          , "\"say \\\"hi\\\"\\n\" : String"
          , "type error: the types do not fit: a number where String \
            \is needed" ]
    -- ANCHOR_END: stringExamples

  -- 「タプルとリスト」
  describe "tuples and lists" $ do
    -- ANCHOR: collections
    -- 「タプルとリストに、型が付いて、計算できる」
    it "gives tuples and lists their types and values" $
      runLines
        [ "(\"Lam\", 42)"
        , "snd (\"Lam\", 42)"
        , ":type []"
        , "1 : 2 : [3]"
        , "map (\\x -> x * x) (range 1 5)"
        , "[1, 2] ++ [3]"
        , "length [1, 2, 3]"
        , "length 5"
        , "[1, true]"
        , "head []" ]
        `shouldBe`
          [ "(\"Lam\", 42) : (String, Int)"
          , "42 : Int"
          , "[a]"
          , "[1, 2, 3] : [Int]"
          , "[1, 4, 9, 16, 25] : [Int]"
          , "[1, 2, 3] : [Int]"
          , "3 : Int"
          , "type error: Int is not in the class HasLength"
          , "type error: the types do not fit: a number where Bool is needed"
          , "error: head: the list is empty" ]
    -- ANCHOR_END: collections

    -- ANCHOR: listPrelude
    -- 「リストの関数に、型が付いている」
    it "gives the list functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["foldr", "map", "filter", "length", "range"]
        `shouldBe` map Just
          [ "Foldable f => (a -> b -> b) -> b -> f a -> b"
          , "Functor f => (a -> b) -> f a -> f b"
          , "(a -> Bool) -> [a] -> [a]"
          , "HasLength a => a -> Int"
          , "(Comparable a, Number a) => a -> a -> [a]" ]
    -- ANCHOR_END: listPrelude

    -- ANCHOR: listLikeHaskell
    -- 「Lam のリストの関数は、Haskell のものと同じ答えを出す」
    it "computes the same lists as Haskell" $
      -- Negative numbers, too: renderList writes -3 as it is.
      -- 負の数も。renderList は -3 をそのまま書く
      property $ forAll (listOf (choose (-9, 9))) $ \xs ->
        let source = "reverse (filter (\\x -> x > 4) "
                     <> renderList xs <> ")"
        in runLines [source]
             == [renderList (reverse (filter (> 4) xs)) <> " : [Int]"]
    -- ANCHOR_END: listLikeHaskell

  -- 「比べる」
  describe "comparing" $
    -- ANCHOR: compare
    -- 「Comparable の型なら比べられる。関数は、動かす前に断る」
    it "compares Comparable values, and refuses functions before running" $
      runLines
        [ "\"apple\" < \"banana\""
        , "[1, 2] < [1, 3]"
        , "(\"Lam\", 1) /= (\"Lam\", 2)"
        , "\"ab\" <= \"b\""
        , ":type \\x y -> x <= y"
        , "(\\x -> x) == (\\x -> x)"
        , "[toUpper] == []"
        , "max toUpper toLower" ]
        `shouldBe`
          [ "true : Bool", "true : Bool", "true : Bool"
          , "true : Bool"
          , "Comparable a => a -> a -> Bool"
          , "type error: a -> a is not in the class Comparable"
          , "type error: String -> String is not in the class Comparable"
          , "type error: String -> String is not in the class Comparable" ]
    -- ANCHOR_END: compare

  -- 「&& と ||」
  describe "&& and ||" $ do
    -- ANCHOR: shortcut
    -- 「&& と || は、要らないときは右側を計算しない」
    it "does not compute the right side when it is not needed" $
      runLines
        [ "and false (1 // 0 == 0)"
        , "false && 1 // 0 == 0"
        , "true || 1 // 0 == 0"
        , "true && 1 // 0 == 0" ]
        `shouldBe`
          [ "error: divided by zero"
          , "false : Bool"
          , "true : Bool"
          , "error: divided by zero" ]
    -- ANCHOR_END: shortcut

    -- ANCHOR: shortcutLikeHaskell
    -- 「&& と || の答えは、Haskell の && と || と同じ」
    it "gives the same answers as Haskell" $
      property $ \a b c ->
        let lam x = if x then "true" else "false"
            source = lam a <> " || " <> lam b <> " && " <> lam c
        in runLines [source] == [lam (a || b && c) <> " : Bool"]
    -- ANCHOR_END: shortcutLikeHaskell

  -- ANCHOR: examples
  -- 「例のプログラム（ゴールデンテスト）」
  describe "examples" $
    mapM_ golden
      [ "fib", "church", "broken", "answers", "strings", "words", "shapes"
      , "sugar"
      -- ANCHOR: recordGolden
      , "records/annotate", "records/report", "records/tuples"
      , "records/crew", "records/shapes", "records/scores"
      , "records/broken/unnoted", "records/broken/noted"
      , "records/broken/first", "records/broken/swap"
      , "records/broken/largest", "records/broken/len"
      , "records/broken/len-late", "records/broken/order"
      -- ANCHOR_END: recordGolden
      -- ANCHOR: floatGolden
      , "float/temps", "float/split", "float/prices"
      , "float/broken/mixed", "float/broken/called"
      -- ANCHOR_END: floatGolden
      -- ANCHOR: classGolden
      , "classes/before", "classes/describe", "classes/byhand"
      , "classes/containers", "classes/combine", "classes/defaults"
      , "classes/ranked", "classes/broken/missing", "classes/broken/unsaid"
      , "classes/broken/ambiguous", "classes/broken/ambiguous-fixed"
      , "classes/broken/super"
      -- ANCHOR_END: classGolden
      -- ANCHOR: jsonGolden
      , "json/grid", "json/orders", "json/tojson"
      -- ANCHOR_END: jsonGolden
      -- ANCHOR: stdlibGolden
      , "stdlib/signup", "stdlib/quiz", "stdlib/tally", "stdlib/team"
      , "stdlib/skipped"
      -- ANCHOR_END: stdlibGolden
      -- ANCHOR: formatGolden
      , "format/money" ]
      -- ANCHOR_END: formatGolden
  -- ANCHOR_END: examples

  -- 「data と case」
  describe "data and case" $ do
    -- ANCHOR: dataExamples
    -- 「型を宣言し、case で値を分ける」
    it "declares types, and takes values apart with case" $
      runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "Circle 2"
        , ":type Rect"
        , "let area s = case s of | Circle r -> 3 * r * r | Rect w h -> w * h"
        , "map area [Circle 2, Rect 3 4]"
        , "case (\"Rust\", 91, true) of | (n, score, _) -> score"
        , "case [1, 2, 3] of | [] -> 0 | x : rest -> x + length rest"
        , "case 5 of | 0 -> \"zero\" | 1 -> \"one\"" ]
        `shouldBe`
          [ "Circle : Int -> Shape\nRect : Int -> Int -> Shape"
          , "Circle 2 : Shape"
          , "Int -> Int -> Shape"
          , "area : Shape -> Int"
          , "[12, 12] : [Int]"
          , "91 : Int"
          , "3 : Int"
          , "error: no branch of case fits 5" ]
    -- ANCHOR_END: dataExamples

    -- ANCHOR: patternTypes
    -- 「パターンのまちがいは、動かす前に見つける」
    it "finds mistakes in patterns before running" $
      drop 1 (runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "case Circle 1 of | Rect w -> w"
        , "case Circle 1 of | Circel r -> r"
        , "case Circle 1 of | Circle \"r\" -> 0"
        , "case Circle 1 of | Circle r -> r | Rect w h -> \"wide\""
        , ":type \\m -> case m of | Nothing -> 0 | Just x -> x" ])
        `shouldBe`
          [ "type error: Rect needs 2 arguments, but the pattern has 1"
          , "type error: unknown constructor: Circel"
          , "type error: the types do not fit: String where Int is needed"
          , "type error: the types do not fit: String where Int is needed"
          , "Number a => Maybe a -> a" ]
    -- ANCHOR_END: patternTypes

    -- ANCHOR: dataErrors
    -- 「data の宣言のまちがい」
    it "finds mistakes in data declarations" $
      runLines
        [ "data T = C Strng"
        , "data T = C a"
        , "data T = C Maybe"
        , "data List a = Nil" ]
        `shouldBe`
          [ "in the data type T: type error: unknown type: Strng"
          , "in the data type T: type error: unknown type variable: a"
          , "in the data type T: type error: the type Maybe needs \
            \1 argument, but got 0"
          , "in the data type List: type error: List is a builtin type" ]
    -- ANCHOR_END: dataErrors

    -- ANCHOR: guards
    -- 「ガードが偽なら、次の枝へ進む」
    it "goes on to the next branch when a guard is false" $
      runLines
        [ "let size n = case n of | 0 -> \"zero\" | k if k < 10 -> \"small\" \
          \| _ -> \"big\""
        , "map size [0, 3, 42]"
        , "let x = 10 in case 50 of | x if x > 100 -> 0 | _ -> x" ]
        `shouldBe`
          [ "size : Int -> String"
          , "[\"zero\", \"small\", \"big\"] : [String]"
          , "10 : Int" ]
    -- ANCHOR_END: guards

    -- ANCHOR: guardSugar
    -- 「ガードは、case と if に開く」
    it "turns guards into case and if" $
      fmap render (parseExpr "case n of | k if k < 10 -> 1 | _ -> 2")
        `shouldBe` Right
          "let $it = n in \
          \let $next = \\$u -> case $it of | _ -> 2 in \
          \case $it of | k -> (if k < 10 then 1 else $next 0) | _ -> $next 0"
    -- ANCHOR_END: guardSugar

    -- ANCHOR: maybeResult
    -- 「プレリュードに Maybe と Result がある」
    it "has Maybe and Result in the prelude" $
      runLines
        [ "lookup \"TS\" [(\"Python\", 82), (\"TS\", 75)]"
        , "lookup \"Go\" [(\"Python\", 82)]"
        , "fromMaybe 0 (lookup \"Go\" [(\"Python\", 82)])"
        , "find even [1, 3, 4, 5]"
        , ":type Err"
        , "[Ok 1, Err \"oops\"]" ]
        `shouldBe`
          [ "Just 75 : Maybe Int"
          , "Nothing : Maybe Int"
          , "0 : Int"
          , "Just 4 : Maybe Int"
          , "a -> Result a b"
          , "[Ok 1, Err \"oops\"] : [Result String Int]" ]

    -- 「Maybe の関数に、型が付いている」
    it "gives the Maybe functions their types" $
      map (\n -> renderScheme <$> Map.lookup n (sessionTypes preludeSession))
        ["fromMaybe", "find", "lookup"]
        `shouldBe` map Just
          [ "a -> Maybe a -> a"
          , "Foldable f => (a -> Bool) -> f a -> Maybe a"
          , "Comparable a => a -> [(a, b)] -> Maybe b" ]
    -- ANCHOR_END: maybeResult

  -- 「自分の型を比べる」
  describe "comparing data" $ do
    -- ANCHOR: compareData
    -- 「Comparable かどうかは、中身から決まる」
    it "decides from the insides whether a type is Comparable" $
      runLines
        [ "data Shape = Circle Int | Rect Int Int"
        , "Circle 5 < Rect 1 1"
        , "Rect 1 2 < Rect 1 3"
        , "Nothing < Just 0"
        , ":type \\x y -> Just x == y"
        , "Just toUpper == Nothing"
        , "data Handler = Button (Int -> Int) | Off"
        , "Off == Off"
        , "data Tagged a = Tagged Int"
        , ":type \\x -> x == Tagged 1"
        , "data Tree a = Leaf | Node (Tree a) a (Tree a)"
        , "Node Leaf 1 Leaf < Node Leaf 2 Leaf"
        , ":type \\x y -> Node Leaf x Leaf == y" ]
        `shouldBe`
          [ "Circle : Int -> Shape\nRect : Int -> Int -> Shape"
          , "true : Bool"
          , "true : Bool"
          , "true : Bool"
          , "Comparable a => a -> Maybe a -> Bool"
          , "type error: String -> String is not in the class Comparable"
          , "Button : (Int -> Int) -> Handler\nOff : Handler"
          , "type error: Handler is not in the class Comparable \
            \(Button holds Int -> Int)"
          , "Tagged : Int -> Tagged a"
          , "Tagged a -> Bool"
          , "Leaf : Tree a\nNode : Tree a -> a -> Tree a -> Tree a"
          , "true : Bool"
          , "Comparable a => a -> Tree a -> Bool" ]
    -- ANCHOR_END: compareData

    -- ANCHOR: compareLikeHaskell
    -- 「Maybe の大小は、Haskell の deriving Ord と同じ」
    it "orders Maybe like Haskell's deriving Ord" $
      property $ forAll ((,) <$> genMaybe <*> genMaybe) $ \(a, b) ->
        let source = lamMaybe a <> " < " <> lamMaybe b
        in runLines [source] == [lamBool (a < b) <> " : Bool"]
    -- ANCHOR_END: compareLikeHaskell

  -- 「パターンを引数に」
  describe "patterns as arguments" $ do
    -- ANCHOR: lambdaSugar
    -- 「パターンの引数は、枝が1つの case に開く」
    it "opens a pattern argument into a case with one branch" $
      map (fmap render . parseExpr)
        [ "\\(a, b) -> a + b"
        , "\\x (Just y) -> x + y"
        , "let (q, r) = (17 // 5, mod 17 5) in q * 10 + r"
        , "let swap (a, b) = (b, a) in swap" ]
        `shouldBe` map Right
          [ "\\$arg -> case $arg of | (a, b) -> a + b"
          , "\\x -> \\$arg -> case $arg of | Just y -> x + y"
          , "case (17 // 5, mod 17 5) of | (q, r) -> q * 10 + r"
          , "let swap = \\$arg -> case $arg of | (a, b) -> (b, a) in swap" ]
    -- ANCHOR_END: lambdaSugar

    -- ANCHOR: patternArgs
    -- 「タプルや構成子を、引数でほどく」
    it "takes tuples and constructors apart in arguments" $
      runLines
        [ "map (\\(name, score) -> name) [(\"Python\", 82), (\"TS\", 75)]"
        , "let (q, r) = (17 // 5, mod 17 5) in (q, r)"
        , "let swap (a, b) = (b, a)"
        , "swap (1, \"one\")"
        , "let area (w, h) = w * h"
        , "(\\(Just x) -> x) Nothing"
        , "let (a, b) = (1, 2)" ]
        `shouldBe`
          [ "[\"Python\", \"TS\"] : [String]"
          , "(3, 2) : (Int, Int)"
          , "swap : (a, b) -> (b, a)"
          , "(\"one\", 1) : (String, Int)"
          , "area : Number a => (a, a) -> a"
          , "error: no branch of case fits Nothing"
          , "<repl>:1:20:\n  |\n1 | let (a, b) = (1, 2)\n\
            \  |                    ^\n\
            \unexpected end of input\nexpecting a value, an operator, \
            \or in (a let with a pattern needs in)\n" ]
    -- ANCHOR_END: patternArgs

    -- ANCHOR: letNotRecursive
    -- 「パターンの let は再帰しない（外の x を使う）」
    it "does not make a pattern let recursive" $
      runLines ["let x = 5 in let (x, y) = (x + 1, x + 2) in (x, y)"]
        `shouldBe` ["(6, 7) : (Int, Int)"]
    -- ANCHOR_END: letNotRecursive

  -- 「演算子を関数に」
  describe "sections" $ do
    -- ANCHOR: sectionSugar
    -- 「演算子とセクションは、ラムダに開く」
    it "opens operators and sections into lambdas" $
      map (fmap render . parseExpr)
        ["(+)", "(+ 1)", "(10 -)", "(- 1)", "(:)", "(x :)"]
        `shouldBe` map Right
          [ "\\$l -> \\$r -> $l + $r"
          , "\\$l -> $l + 1"
          , "\\$r -> 10 - $r"
          , "\\$l -> $l - 1"
          , "\\$l -> \\$r -> $l : $r"
          , "\\$r -> x : $r" ]
    -- ANCHOR_END: sectionSugar

    -- ANCHOR: sections
    -- 「セクションを使う。(-1) はセクションではなく、負の数」
    it "uses sections, and reads (-1) as a negative number" $
      runLines
        [ "map (* 2) [1, 2, 3]"
        , "(10 -) 3"
        , "(- 1) 10"
        , "foldr (+) 0 [1, 2, 3]"
        , ":type (++)"
        , "(-1)" ]
        `shouldBe`
          [ "[2, 4, 6] : [Int]"
          , "7 : Int"
          , "9 : Int"
          , "6 : Int"
          , "Semigroup a => a -> a -> a"
          , "-1 : Int" ]
    -- ANCHOR_END: sections

  -- 「リスト内包表記」
  describe "list comprehensions" $ do
    -- ANCHOR: comprehensionSugar
    -- 「内包表記は、ループ $go に開く」
    it "opens a comprehension into a loop $go" $
      fmap render (parseExpr "[x * 2 | x <- xs, x > 1]")
        `shouldBe` Right
          "let $go = \\$xs -> case $xs of \
          \| [] -> [] \
          \| x : $rest -> (if x > 1 then [x * 2] else []) ++ $go $rest \
          \in $go xs"
    -- ANCHOR_END: comprehensionSugar

    -- ANCHOR: comprehensions
    -- 「<- をいくつも、条件、let、外れうるパターン」
    it "has many <-, conditions, let, and patterns that may not fit" $
      runLines
        [ "[x * 2 | x <- [1, 2, 3, 4], x > 1]"
        , "[(x, y) | x <- [1, 2, 3], y <- [1, 2, 3], x < y]"
        , "[(x, y) | x <- [1, 2], y <- [x, 10]]"
        , "[y | x <- [1, 2, 3], let y = x * 10, y > 15]"
        , "[x | Just x <- [Just 1, Nothing, Just 3]]"
        , ":type \\xs -> [(x, y) | x <- xs, y <- xs]"
        , "[x | x <- 5]" ]
        `shouldBe`
          [ "[4, 6, 8] : [Int]"
          , "[(1, 2), (1, 3), (2, 3)] : [(Int, Int)]"
          , "[(1, 1), (1, 10), (2, 2), (2, 10)] : [(Int, Int)]"
          , "[20, 30] : [Int]"
          , "[1, 3] : [Int]"
          , "[a] -> [(a, a)]"
          , "type error: the types do not fit: a number where [a] is needed" ]
    -- ANCHOR_END: comprehensions

    -- ANCHOR: comprehensionLikeHaskell
    -- 「Haskell の内包表記と同じ答えになる」
    it "gives the same answers as Haskell's comprehensions" $
      property $ forAll (listOf (choose (0, 9))) $ \xs ->
        forAll (listOf (choose (0, 9))) $ \ys ->
          let source = "[x * 10 + y | x <- " <> renderList xs
                         <> ", x > 2, y <- " <> renderList ys
                         <> ", let s = x + y, mod s 2 == 0]"
              expected = [ x * 10 + y | x <- xs, x > 2, y <- ys
                                      , let s = x + y, s `mod` 2 == 0 ]
          in runLines [source] == [renderList expected <> " : [Int]"]
    -- ANCHOR_END: comprehensionLikeHaskell

  -- 「パイプラインと合成」
  describe "pipes and composition" $ do
    -- ANCHOR: pipeSugar
    -- 「|> と >> も、App と Lam に開く」
    it "opens |> and >> into App and Lam" $
      map (fmap render . parseExpr)
        ["xs |> map f |> sum", "sum <| map f <| xs", "f >> g", "g << f"]
        `shouldBe` map Right
          [ "sum (map f xs)"
          , "sum (map f xs)"
          , "\\$x -> g (f $x)"
          , "\\$x -> g (f $x)" ]
    -- ANCHOR_END: pipeSugar

    -- ANCHOR: pipes
    -- 「左から右へ流す、関数をつなぐ」
    it "flows from left to right, and joins functions" $
      runLines
        [ "[1, 2, 3] |> map (* 2) |> sum"
        , "((+ 1) >> (* 2)) 5"
        , "((+ 1) << (* 2)) 5"
        , ":type (>>)"
        , "[\"b\", \"a\"] |> map (toUpper >> chars) |> length"
        , "case 3 |> (+ 1) of | 4 -> \"four\" | _ -> \"other\"" ]
        `shouldBe`
          [ "12 : Int"
          , "12 : Int"
          , "11 : Int"
          , "(a -> b) -> (b -> c) -> a -> c"
          , "2 : Int"
          , "\"four\" : String" ]
    -- ANCHOR_END: pipes

  -- 「型の注釈とレコード」
  describe "annotations and records" $ do
    -- ANCHOR: annotationTest
    -- 「let のない定義、注釈、where を読む。ファイルの終わりの x : xs は式」
    it "reads definitions without let, annotations and where" $ do
      let decls src = fmap (\(ds, final) ->
            ( [ case d of
                  DefValue x e -> x <> " = " <> render e
                  DefSig x _   -> x <> " : ..."
                  _            -> "?"
              | d <- ds ]
            , fmap render final ))
            (runLamParser ops programParts "" src)
      decls "add : Int -> Int -> Int\nadd x y = x + y\n"
        `shouldBe` Right (["add : ...", "add = \\x -> \\y -> x + y"], Nothing)
      decls "f x = a * b\n  where\n    a = x + 1\n    b = 2\n"
        `shouldBe` Right
          (["f = \\x -> let a = x + 1 in let b = 2 in a * b"], Nothing)
      decls "xs = [2]\nx = 1\nx : xs\n"
        `shouldBe` Right (["xs = [2]", "x = 1"], Just "x : xs")
    -- ANCHOR_END: annotationTest

    -- ANCHOR: recordTypesTest
    -- 「レコードの型：読むだけなら、ほかのフィールドがあってよい」
    it "gives the types of records, with the rest of the fields" $
      runLines
        [ "{ name = \"Lam\", age = 3 }"
        , ":type \\r -> r.name"
        , ":type \\r -> (r.name, r.age)"
        , ":type \\r -> { r | age = r.age + 1 }"
        , ":type .name"
        , ":type .name >> toUpper"
        , ":type \\{ x, y } -> x + y"
        , "(\\r -> r.name) { name = \"Ruby\", version = \"4.0\" }"
        , "{ name = \"Lam\" }.age"
        , "{ name = \"Lam\" } == { name = \"Lam\" }"
        , "{ f = \\x -> x } == { f = \\x -> x }" ]
        `shouldBe`
          [ "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "{ r | name : a } -> a"
          , "{ r | age : a, name : b } -> (b, a)"
          , "Number a => { r | age : a } -> { r | age : a }"
          , "{ r | name : a } -> a"
          , "{ r | name : String } -> String"
          , "Number a => { r | x : a, y : a } -> a"
          , "\"Ruby\" : String"
          , "type error: { name : String } has no field age"
          , "true : Bool"
          , "type error: a -> a is not in the class Comparable" ]
    -- ANCHOR_END: recordTypesTest

    -- 「本文の REPL のやり取り」
    it "answers as the chapter shows" $
      runLines
        [ "r = { name = \"Lam\", age = 3 }", "r.name", "{ r | age = 4 }", "r"
        , "r.title", "{ r | title = \"x\" }", "{ r | age = \"four\" }"
        , "label r = \"[\" ++ r.name ++ \"]\"", "label r"
        , "label { name = \"Ruby\", version = \"4.0\" }", "label { title = \"x\" }"
        , "total : [Int] -> Int; total xs = sum xs"
        , ":type max", "biggest : Int -> Int -> Int; biggest = max"
        , "{ name = \"Lam\" } 1"
        , "half : Int -> Int; double x = x * 2" ]
        `shouldBe`
          [ "r : Number a => { age : a, name : String }\n\
            \  -- usually : { age : Int, name : String }"
          , "\"Lam\" : String"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "{ age = 3, name = \"Lam\" } : { age : Int, name : String }"
          , "type error: { age : a, name : String } has no field title"
          , "type error: { age : a, name : String } has no field title"
          , "type error: the types do not fit: a number where String \
            \is needed"
          , "label : { r | name : String } -> String"
          , "\"[Lam]\" : String"
          , "\"[Ruby]\" : String"
          , "type error: { title : String } has no field name"
          , "total : [Int] -> Int"
          , "Comparable a => a -> a -> a"
          , "biggest : Int -> Int -> Int"
          , "type error: the types do not fit: { name : String } \
            \where a -> b is needed"
          , "<repl>:1:36:\n  |\n1 | half : Int -> Int; double x = x * 2\n  |                                    ^\nthe annotation and the definition have different names\n" ]

    -- ANCHOR: updateProp
    -- 「一部を変えたレコード：変えたフィールドは新しい値、ほかはそのまま」
    it "changes only the fields it names" $
      property $ \a b n ->
        let r = "{ a = " <> num a <> ", b = " <> num b <> " }"
            changed = "{ r | a = " <> num n <> " }"
        in map valuePart (runLines
             [ inRecord r (changed <> ".a")
             , inRecord r (changed <> ".b")
             , inRecord r "r.a" ])
           === map (T.pack . show) [n, b, a]
    -- ANCHOR_END: updateProp

    -- ANCHOR: aliasTest
    -- 「別名は、中身の型と同じ」
    it "takes an alias as the type it stands for" $
      runLines
        [ "type alias User = { name : String, age : Int }"
        , "older : User -> User; older u = { u | age = u.age + 1 }"
        , "older { name = \"Lam\", age = 3 }"
        , "type alias Pair a = (a, a)"
        , "both : Pair Int -> Int; both (x, y) = x + y"
        , "type alias Bad = Bad"
        , "type alias Int = Bool" ]
        `shouldBe`
          [ "type alias User"
          , "older : { age : Int, name : String } -> { age : Int, name : String }"
          , "{ age = 4, name = \"Lam\" } : { age : Int, name : String }"
          , "type alias Pair"
          , "both : (Int, Int) -> Int"
          , "in the type alias Bad: type error: unknown type: Bad"
          , "in the type alias Int: type error: Int is a builtin type" ]
    -- ANCHOR_END: aliasTest

    -- 「注釈の下に、同じ名前の定義が要る」
    it "needs the definition right below an annotation" $
      fmap snd (loadPlain preludeSession "<test>" "half : Int -> Int\ndouble x = x * 2\n")
        `shouldBe` Left "error: the type annotation of half needs the definition of half right below it"

  -- 「キーワードで始まる名前」
  describe "names" $
    -- ANCHOR: keywordStart
    -- 「キーワードで始まる名前も読める（iffy、letter、cases、dataset、
    --   double、done）」
    it "reads names that start with a keyword" $
      runLines
        [ "let iffy = 1", "iffy + 1", "let letter = \"a\"", "letter"
        , "let cases = 2", "cases * 3", "let dataset = true", "dataset"
        , "double x = x * 2", "double 4", "done = true", "done" ]
        `shouldBe`
          [ "iffy : Number a => a\n  -- usually : Int", "2 : Int"
          , "letter : String", "\"a\" : String"
          , "cases : Number a => a\n  -- usually : Int", "6 : Int"
          , "dataset : Bool", "true : Bool"
          , "double : Number a => a -> a", "8 : Int"
          , "done : Bool", "true : Bool" ]
    -- ANCHOR_END: keywordStart

  -- 「小数」
  describe "floats" $ do
    -- ANCHOR: floatParse
    -- 「数のリテラルと負の数を読む。マイナスの前後の空白で見分ける」
    it "reads Floats and negative numbers, by the spaces around -" $
      map (fmap render . parseExpr)
        [ "1.5", "1e3", "2.5e-3", "-7", "-1.5", "f -7", "x - 1", "x-1"
        , "x -1", "-x", "-(a + b)", "a * -2", "[-1, -2]", "(-1)", "(- 1)"
        , "-r.age", "Float.isNaN x" ]
        `shouldBe` map Right
          [ "1.5", "1000.0", "0.0025", "-7", "-1.5", "f -7", "x - 1"
          , "x - 1", "x -1", "negate x", "negate (a + b)", "a * -2"
          , "[-1, -2]", "-1", "\\$l -> $l - 1", "negate r.age"
          , "Float.isNaN x" ]
    -- ANCHOR_END: floatParse

    -- ANCHOR: floatShow
    -- 「Float は、JavaScript と同じ形で見せる（node で確かめた）」
    it "shows a Float as JavaScript does" $
      map showFloat
        [ 1, 0.1 + 0.2, 1e21, 1e-7, 123456789012345680000, 0.000001
        , 1.5e-10, -2.5, 100, 1 / 3, 5e-324, 1.7976931348623157e308
        , 2 ^ (53 :: Int), 0 / 0, 1 / 0, -1 / 0 ]
        `shouldBe`
          [ "1", "0.30000000000000004", "1e+21", "1e-7"
          , "123456789012345680000", "0.000001", "1.5e-10", "-2.5", "100"
          , "0.3333333333333333", "5e-324", "1.7976931348623157e+308"
          , "9007199254740992", "NaN", "Infinity", "-Infinity" ]
    -- ANCHOR_END: floatShow

    -- ANCHOR: floatLiteralProp
    -- 「どんな Float のリテラルも、書いて読み直すと同じ Float」
    it "reads back any Float literal it writes" $
      property $ \x ->
        parseExpr (render (Lit (LFloat x))) == Right (Lit (LFloat x))
    -- ANCHOR_END: floatLiteralProp

    -- ANCHOR: floatLikeHaskell
    -- 「+ - * / の答えは、Haskell の Double と同じ」
    it "computes + - * / as Haskell's Double does" $
      property $ \a b ->
        let lit x = "(" <> floatLiteral x <> ")"
            run o = runLines [lit a <> " " <> o <> " " <> lit b]
        in map run ["+", "-", "*", "/"]
             === map (\x -> [showFloat x <> " : Float"])
                     [a + b, a - b, a * b, a / (b :: Double)]
    -- ANCHOR_END: floatLikeHaskell

    -- ANCHOR: intDivLikeHaskell
    -- 「// は Haskell の div、//? は 0 のときだけ Nothing」
    it "divides Ints as Haskell's div does" $
      property $ \a b ->
        runLines [num a <> " // " <> num b, num a <> " //? " <> num b]
          === if b == 0
                then ["error: divided by zero", "Nothing : Maybe Int"]
                else [ T.pack (show (a `div` b)) <> " : Int"
                     , "Just " <> num (a `div` b) <> " : Maybe Int" ]
    -- ANCHOR_END: intDivLikeHaskell

    -- ANCHOR: roundingProp
    -- 「丸めの3つのやり方は、半分のときだけちがう」
    it "rounds a half in three ways, and the rest alike" $
      property $ \(n :: Integer) (k :: Int) ->
        let x = fromInteger n + fromIntegral (k `mod` 10) / 10 :: Double
            lit = "(" <> floatLiteral x <> ")"
            answers = runLines
              [ "round " <> lit, "Float.roundTiesEven " <> lit
              , "Float.roundTiesUp " <> lit ]
            just m = "Just " <> num m <> " : Maybe Int"
            r = toRational x
            away = if r < 0 then negate (round' (negate r)) else round' r
            round' q = floor (q + 1 / 2) :: Integer
        in answers === [just away, just (round r), just (floor (r + 1 / 2))]
    -- ANCHOR_END: roundingProp

    -- ANCHOR: floatRepl
    -- 「本文の REPL のやり取り」
    it "answers the REPL lines of the chapter" $
      runLines
        [ "7 / 2", "1.5 + 2", "0.1 + 0.2", "1e3", "-7", "abs -7"
        , "(-1)", "(- 1) 10", "map (- 1) [10, 20]"
        , "x = 10", "x - 1", "x-1", "x -1", "-(x + 1)"
        , "double n = n * 2", "double 4", "double 1.5"
        , "length [1, 2] * 1.5", "toFloat (length [1, 2]) * 1.5"
        , "\"a\" + 1", "1 + 2", ":type 1 + 2", "n = 3"
        , "7 // 2", "-7 // 2", "7 // 0", "7 //? 0", "1 / 0", "0 / 0"
        , "7 /? 2", "round 2.5", "round -2.5", "Float.roundTiesEven 2.5"
        , "Float.roundTiesUp -2.5", "floor -2.7", "truncate -2.7"
        , "round (0 / 0)", "round 2.5 ?: 0", "Nothing ?: Just 3 ?: 0"
        , "Just 1 ?: 1 // 0", "Err \"no\" ?: 0", "Ok 2 ?: 0"
        , ":type \\m -> m ?: 0"
        , "nan = 0 / 0", "nan == nan", "Float.isNaN nan"
        , "showFloat (2 / 3)", "Float.fixed 3 (2 / 3)", "Float.fixed 1 -0.25"
        , "Float.fixed 0 2.5", "Float.fixed 2 7"
        , "next : a -> a; next x = x + 1"
        , "next : Number a => a -> a; next x = x + 1"
        , "3 == 3.0", "toFloat 3", "sqrt 2", "Float.pow 2 10" ]
        `shouldBe`
          [ "3.5 : Float", "3.5 : Float", "0.30000000000000004 : Float"
          , "1000 : Float", "-7 : Int", "7 : Int"
          , "-1 : Int", "9 : Int", "[9, 19] : [Int]"
          , "x : Number a => a\n  -- usually : Int", "9 : Int", "9 : Int"
          , "type error: a number and a function are mixed up \
            \(a -> b is not in the class Number): x -1 gives -1 to x \
            \(write x - 1), and -f x is (-f) x (write -(f x))"
          , "-11 : Int"
          , "double : Number a => a -> a", "8 : Int", "3 : Float"
          , "type error: the types do not fit: Float where Int is needed"
          , "3 : Float"
          , "type error: the types do not fit: a number where String \
            \is needed"
          , "3 : Int", "Number a => a\n  -- usually : Int"
          , "n : Number a => a\n  -- usually : Int"
          , "3 : Int", "-4 : Int", "error: divided by zero"
          , "Nothing : Maybe Int", "Infinity : Float", "NaN : Float"
          , "Just 3.5 : Maybe Float", "Just 3 : Maybe Int"
          , "Just (-3) : Maybe Int", "Just 2 : Maybe Int"
          , "Just (-2) : Maybe Int", "Just (-3) : Maybe Int"
          , "Just (-2) : Maybe Int", "Nothing : Maybe Int", "3 : Int"
          , "3 : Int", "1 : Int", "0 : Int", "2 : Int"
          , "(Fallible f, Number a) => f a -> a"
          , "nan : Float", "false : Bool", "true : Bool"
          , "\"0.6666666666666666\" : String", "\"0.667\" : String"
          , "\"-0.3\" : String", "\"3\" : String", "\"7.00\" : String"
          , "in the definition of next: type error: the \
            \annotation of next must say Number a (the definition needs it)"
          , "next : Number a => a -> a", "true : Bool", "3 : Float"
          , "1.4142135623730951 : Float", "1024 : Float" ]
    -- ANCHOR_END: floatRepl

    -- ANCHOR: splitRepl
    -- 「割り勘の例を :load して、share を試す」
    it "splits the bill in the example" $ do
      source <- Utf8.readFile "examples/float/split.lam"
      let loaded = case loadPlain preludeSession "split.lam" source of
            Right (s, _) -> s
            Left err     -> error (T.unpack err)
          go (s, outs) line =
            let (s', out) = replStep s line in (s', outs <> [out])
      snd (foldl go (loaded, []) ["share 4", "share 0", "exact 4"])
        `shouldBe`
          [ "Just (622, 2) : Maybe (Int, Int)", "Nothing : Maybe (Int, Int)"
          , "\"622.50: 623 622 623\" : String" ]
    -- ANCHOR_END: splitRepl

    -- ANCHOR: orElseErrors
    -- 「?: の左が Maybe でも Result でもなければ、そう教える」
    it "says what ?: needs on its left" $
      runLines ["3 ?: 0", "\"a\" ?: \"b\"", "[1] ?: 0", "round 2.5 ?: 0"]
        `shouldBe`
          [ "type error: ?: needs a Maybe or a Result on its left, but got \
            \a number (only a Maybe or a Result can be missing its value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got \
            \String (only a Maybe or a Result can be missing its value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got \
            \[a] (only a Maybe or a Result can be missing its value)"
          , "3 : Int" ]
    -- ANCHOR_END: orElseErrors

  -- 「複数行」
  -- 「クラスとインスタンス」
  describe "classes" $ do
    -- ANCHOR: classParse
    -- 「class の下に、メソッドの型と既定のメソッドを並べて読む」
    it "reads the methods and the defaults under a class" $
      fmap (\(decls, _) ->
              [ (classDeclName c, classVar c, map fst (classMethods c)
                , map fst (classDefaults c))
              | DefClass c <- decls ])
        (runLamParser ops programParts ""
           "class Describe a where\n\
           \  describe : a -> String\n\
           \  describeAll : [a] -> String\n\
           \  describeAll xs = join \", \" (map describe xs)\n\
           \describe\n")
        `shouldBe` Right
          [("Describe", "a", ["describe", "describeAll"], ["describeAll"])]
    -- ANCHOR_END: classParse

    -- ANCHOR: classRepl
    -- 「本文の REPL のやり取り（1行で書けるクラス）」
    it "answers the REPL lines of the chapter" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "describe 42", ":type describe", ":dicts describe 42"
        , "describe true"
        , "describeTwice x = describe x ++ \", \" ++ describe x"
        , ":dicts describeTwice 7"
        , ":dicts \\x -> describe x ++ describe x" ]
        `shouldBe`
          [ "describe : Describe a => a -> String"
          , "instance Describe Int"
          , "\"42\" : String", "Describe a => a -> String"
          , "describe $Describe$Int 42"
          , "type error: Bool is not in the class Describe"
          , "describeTwice : Describe a => a -> String"
          , "describeTwice $Describe$Int 7"
          , "\\$Describe_a -> \\x -> describe $Describe_a x \
            \++ describe $Describe_a x" ]
    -- ANCHOR_END: classRepl

    -- ANCHOR: classDicts
    -- 「例を :load して、辞書を入れた式を見る」
    it "shows where the dictionaries go" $ do
      containers <- replAfter "examples/classes/containers.lam"
        [":dicts describe (Just [3, 4])"]
      ranked <- replAfter "examples/classes/ranked.lam"
        [ ":dicts \\x -> describe x ++ showInt (rank x)"
        , ":type \\x -> describe x ++ showInt (rank x)" ]
      combine <- replAfter "examples/classes/combine.lam"
        [":dicts total [Yen 1]", ":dicts total [1, 2]", "total []"]
      (containers, ranked, combine) `shouldBe`
        ( [ "describe ($Describe$Maybe ($Describe$List $Describe$Int)) \
            \(Just [3, 4])" ]
        , [ "\\$Ranked_a -> \\x -> describe $Ranked_a.$Describe x \
            \++ showInt (rank $Ranked_a x)"
          , "Ranked a => a -> String" ]
        , [ "total $Combine$Money [Yen 1]", "total $Combine$Int [1, 2]"
          , "type error: cannot choose an instance of Combine: the type a \
            \is not known here (an annotation can tell it)" ] )
    -- ANCHOR_END: classDicts

    -- 「本文の、例を :load してからのやり取り」
    it "answers the REPL lines after loading the examples" $ do
      described <- replAfter "examples/classes/describe.lam"
        [ ":type describe", ":type twice", ":dicts describe (Yen 500)"
        , ":dicts twice (Yen 80)" ]
      contained <- replAfter "examples/classes/containers.lam"
        [":dicts describe [Just (Circle { radius = 1 }), Nothing]"]
      combined <- replAfter "examples/classes/combine.lam"
        [ ":type empty", ":dicts total [Yen 300, Yen 450]"
        , ":dicts combine empty (Minutes 5)", ":dicts total [1, 2, 3]"
        , "total [1, 2, 3]" ]
      (described, contained, combined) `shouldBe`
        ( [ "Describe a => a -> String", "Describe a => a -> String"
          , "describe $Describe$Money (Yen 500)"
          , "twice $Describe$Money (Yen 80)" ]
        , [ "describe ($Describe$List ($Describe$Maybe $Describe$Shape)) \
            \[Just (Circle { radius = 1 }), Nothing]" ]
        , [ "Combine a => a", "total $Combine$Money [Yen 300, Yen 450]"
          , "combine $Combine$Minutes (empty $Combine$Minutes) (Minutes 5)"
          , "total $Combine$Int [1, 2, 3]", "6 : Int" ] )

    -- ANCHOR: byHandProp
    -- 「Lam が入れる辞書は、手で書いた辞書と同じ答えを出す」
    it "gives the same answers as the dictionaries by hand" $
      property $ \(NonNegative r) (NonNegative n) -> ioProperty $ do
        let circle = "(Circle { radius = " <> num r <> " })"
            line   = "(twice " <> circle <> ", twice (Yen " <> num n <> "))"
            byHand = "(twice shapeDict " <> circle <> ", twice moneyDict (Yen "
                       <> num n <> "))"
        withClass <- replAfter "examples/classes/describe.lam" [line]
        withHand  <- replAfter "examples/classes/byhand.lam" [byHand]
        pure (withClass === withHand)
    -- ANCHOR_END: byHandProp

    -- ANCHOR: totalProp
    -- 「total は、Int なら Haskell の sum と同じ。お金も分も、中の数の合計」
    it "adds up as Haskell's sum does" $
      property $ \(NonEmpty xs) -> ioProperty $ do
        outs <- replAfter "examples/classes/combine.lam"
          [ "total " <> renderList xs
          , "total (map Yen " <> lamList xs <> ")" ]
        pure (outs === [ T.pack (show (sum xs)) <> " : Int"
                       , "Yen " <> num (sum xs) <> " : Money" ])
    -- ANCHOR_END: totalProp

    -- ANCHOR: classSelf
    -- 「注釈のない定義も、同じ辞書で自分を呼ぶ。閉じた中の let は、
    --   宣言したクラスも一般にする（言葉を補強する章から）」
    it "calls itself with the same dictionary, and makes a closed \
       \local let general" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "describeEach xs = case xs of | [] -> \"\" \
          \| x : rest -> describe x ++ describeEach rest"
        , "describeEach [1, 2, 3]"
        , "let f x = describe x in (f 1, f 2)"
        , "let f x = describe x in (f 1, f \"a\")" ]
        `shouldBe`
          [ "describe : Describe a => a -> String", "instance Describe Int"
          , "describeEach : Describe a => [a] -> String", "\"123\" : String"
          , "(\"1\", \"2\") : (String, String)"
          , "type error: String is not in the class Describe" ]
    -- ANCHOR_END: classSelf

    -- 「組み込みのクラスも、上位のクラスに書ける（辞書はない）」
    it "takes a builtin class above a declared one" $
      runLines
        [ "class Comparable a => Sorted a where key : a -> Int"
        , "instance Sorted Int where key n = n"
        , "smaller : Sorted a => a -> a -> a; smaller x y = if x < y then x else y"
        , "smaller 3 5", "data H = H (Int -> Int)"
        , "instance Sorted H where key h = 0" ]
        `shouldBe`
          [ "key : Sorted a => a -> Int", "instance Sorted Int"
          , "smaller : Sorted a => a -> a -> a", "3 : Int"
          , "H : (Int -> Int) -> H"
          , "in the instance Sorted H: type error: H is not in the class Comparable (H holds Int -> Int)" ]

    -- ANCHOR: classErrors
    -- 「クラスとインスタンスのまちがいを、そう教える」
    it "says what is wrong with a class or an instance" $
      runLines
        [ "class Describe a where describe : a -> String"
        , "instance Describe Int where describe n = showInt n"
        , "class Describe a where describe : a -> Int"
        , "instance Comparable Int"
        , "instance Describe Int where describe n = \"x\""
        , "instance Describe Bool where descrbe b = \"b\""
        , "instance Describe Bool"
        , "class Sized a where size : Int"
        , "instance Describe (Int -> Int) where describe f = \"f\""
        , "instance Describe (Maybe Int) where describe m = \"m\"" ]
        `shouldBe`
          [ "describe : Describe a => a -> String", "instance Describe Int"
          , "in the class Describe: type error:\
            \ the class Describe is already declared"
          , "in the instance Comparable Int: type error:\
            \ Comparable is a builtin class: the language decides its\
            \ members"
          , "in the instance Describe Int: type error:\
            \ there is already an instance Describe Int"
          , "in the instance Describe Bool: type error:\
            \ the class Describe has no method descrbe"
          , "in the instance Describe Bool: type error:\
            \ the method describe of Describe is not written (and has no\
            \ default)"
          , "in the class Sized: type error:\
            \ the type of the method size must have the class's a"
          , "in the instance Describe (Int -> Int): type error:\
            \ an instance is for a type name with different type\
            \ variables, like (Maybe a), not Int -> Int"
          , "in the instance Describe (Maybe Int): type error:\
            \ an instance is for a type name with different type\
            \ variables, like (Maybe a), not Maybe Int" ]
    -- ANCHOR_END: classErrors

  -- 「Map」
  describe "maps" $ do
    -- ANCHOR: maps
    -- 「Map を作り、引き、比べる」
    it "makes, looks up and compares maps" $
      runLines
        [ "let scores = Map.insert \"TS\" 75 "
            <> "(Map.insert \"Python\" 82 Map.empty)"
        , "scores"
        , "Map.lookup \"TS\" scores"
        , "Map.lookup \"Go\" scores"
        , "{\"TS\": 75, \"Python\": 82} == scores"
        , ":type {}"
        , "{\"a\": 1} < {\"a\": 2}"
        , "{\"a\": toUpper} == {}"
        , "Map.insertWith (\\a b -> a + b) \"the\" 1 {\"the\": 2}"
        , "let name = \"TS\" in {name: 1, [name]: 2}"
        , "{[1]: 2}" ]
        `shouldBe`
          [ "scores : Number a => Map String a\n  -- usually : Map String Int"
          , "{\"Python\": 82, \"TS\": 75} : Map String Int"
          , "Just 75 : Maybe Int"
          , "Nothing : Maybe Int"
          , "true : Bool"
          , "KeyComparable a => Map a b"
          , "true : Bool"
          , "type error: String -> String is not in the class Comparable"
          , "{\"the\": 3} : Map String Int"
          , "{\"TS\": 2, \"name\": 1} : Map String Int"
          , "{1: 2} : Map Int Int" ]
    -- ANCHOR_END: maps

    -- ANCHOR: qualified
    -- 「Map. の付いた名前は、1つの名前。{ } は Map.fromList の糖衣構文」
    it "reads Map.insert as one name, and { } as sugar" $ do
      parseExpr "Map.insert \"a\" 1 m"
        `shouldBe` Right (App (App (App (Var "Map.insert")
                     (Lit (LString "a"))) (Lit (LInt 1))) (Var "m"))
      fmap render (parseExpr "{\"a\": 1, \"b\": x + 1}")
        `shouldBe` Right "Map.fromList [(\"a\", 1), (\"b\", x + 1)]"
      fmap render (parseExpr "{}") `shouldBe` Right "Map.fromList []"
      -- 「名前の形のキーは、" を省ける（変数ではなく、ただの文字列）」
      fmap render (parseExpr "{name: 1, if: 2, \"two words\": 3}")
        `shouldBe` Right
          "Map.fromList [(\"name\", 1), (\"if\", 2), (\"two words\", 3)]"
      -- 「[ ] で包んだキーは、式の値（変数 name の中身）」
      fmap render (parseExpr "{[name]: 1, [toUpper k]: 2}")
        `shouldBe` Right "Map.fromList [(name, 1), (toUpper k, 2)]"
    -- ANCHOR_END: qualified

    -- ANCHOR: mapLikeHaskell
    -- 「Map.fromList は、Haskell の Map.fromList と同じ答え」
    --  (the last of the same keys wins, and the keys come out in order)
    it "builds the same map as Haskell's Map.fromList" $
      property $ forAll (listOf1 genPair) $ \kvs ->
        let source = "Map.toList (Map.fromList " <> lamPairs kvs <> ")"
        in runLines [source]
             == [lamPairs (Map.toList (Map.fromList kvs))
                   <> " : [(String, Int)]"]
    -- ANCHOR_END: mapLikeHaskell

    -- ANCHOR: anyKeys
    -- 「キーは、Comparable な型なら何でもよい」
    it "takes keys of any Comparable type" $
      runLines
        [ "{1: \"one\", 2: \"two\"}"
        , "Map.lookup (1, 2) {[(1, 2)]: \"a\", [(0, 5)]: \"b\"}"
        , ":type Map.insert"
        , "{-1: 2, 1.5: 3}"
        , "Map.fromList [(Just 2, 1), (Nothing, 0)]"
        , "{[toUpper]: 1}"
        , "{1: 2, \"a\": 3}" ]
        `shouldBe`
          [ "{1: \"one\", 2: \"two\"} : Map Int String"
          , "Just \"a\" : Maybe String"
          , "KeyComparable a => a -> b -> Map a b -> Map a b"
          , "{-1: 2, 1.5: 3} : Map Float Int"
          , "{[Nothing]: 0, [Just 2]: 1} : Map (Maybe Int) Int"
          , "type error: String -> String is not in the class KeyComparable"
          , "type error: the types do not fit: a number where String \
            \is needed" ]
    -- ANCHOR_END: anyKeys

    -- ANCHOR: sets
    -- 「Set：どの値も1つずつ、順に並べて持つ」
    it "keeps each value once, in order, in a set" $
      runLines
        [ "Set.fromList [3, 1, 3, 2]"
        , "Set.member 2 (Set.fromList [1, 2])"
        , "Set.union (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.intersection (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.difference (Set.fromList [1, 2]) (Set.fromList [2, 3])"
        , "Set.toList (Set.insert (0, 0) Set.empty)"
        , "Just (Set.fromList [\"a\"])"
        , "Set.fromList [1, 2] == Set.fromList [2, 1, 2]"
        , ":type Set.fromList"
        , "Set.fromList [toUpper]" ]
        `shouldBe`
          [ "Set.fromList [1, 2, 3] : Set Int"
          , "true : Bool"
          , "Set.fromList [1, 2, 3] : Set Int"
          , "Set.fromList [2] : Set Int"
          , "Set.fromList [1] : Set Int"
          , "[(0, 0)] : [(Int, Int)]"
          , "Just (Set.fromList [\"a\"]) : Maybe (Set String)"
          , "true : Bool"
          , "KeyComparable a => [a] -> Set a"
          , "type error: String -> String is not in the class KeyComparable" ]

    -- 「Set.fromList は、Haskell の Set.fromList と同じ答え」
    it "builds the same set as Haskell's Set.fromList" $
      property $ \(NonEmpty (xs :: [Integer])) ->
        let shown ns = "[" <> T.intercalate ", " (map (T.pack . show) ns)
                         <> "]"
        in runLines ["Set.toList (Set.fromList " <> lamList xs <> ")"]
             === [shown (Set.toList (Set.fromList xs)) <> " : [Int]"]
    -- ANCHOR_END: sets

    -- ANCHOR: mapTypes
    -- 「型の注釈と data の宣言にも、Map と Set を書ける」
    it "writes Map and Set in annotations and data" $
      runLines
        [ "count : [String] -> Map String Int; count ws = "
            <> "foldl (\\m w -> Map.insertWith (+) w 1 m) {} ws"
        , "count [\"a\", \"b\", \"a\"]"
        , "data Board = Board (Set (Int, Int))"
        , "Board (Set.fromList [(1, 2)])"
        , "data T = C (Map Int)" ]
        `shouldBe`
          [ "count : [String] -> Map String Int"
          , "{\"a\": 2, \"b\": 1} : Map String Int"
          , "Board : Set (Int, Int) -> Board"
          , "Board (Set.fromList [(1, 2)]) : Board"
          , "in the data type T: type error: the type Map needs 2 \
            \arguments, but got 1" ]
    -- ANCHOR_END: mapTypes

    -- ANCHOR: nanKeys
    -- 「NaN のキーも、また引ける。== は IEEE どおり」
    it "finds a NaN key again" $
      runLines
        [ "nan = 0 / 0"
        , "m = Map.insert nan \"a\" Map.empty"
        , "Map.lookup nan m"
        , "Map.insert nan \"b\" m"
        , "Set.fromList [nan, nan, 1.5, -1 / 0]"
        , "Set.member (Just nan) (Set.fromList [Just nan])"
        , "nan == nan"
        , "f : Comparable k => k -> Map k Int; f k = Map.insert k 1 {}"
        , "g k = (k < k, Map.insert k 1 {})" ]
        `shouldBe`
          [ "nan : Float"
          , "m : Map Float String"
          , "Just \"a\" : Maybe String"
          , "{NaN: \"b\"} : Map Float String"
          , "Set.fromList [-Infinity, 1.5, NaN] : Set Float"
          , "true : Bool"
          , "false : Bool"
          , "f : Comparable a => a -> Map a Int"
          , "g : (Comparable a, Number b) => a -> (Bool, Map a b)" ]

    -- 「キーの比べ方は、NaN のほかは == と < と同じ」
    it "compares keys as == and < do, but for NaN" $
      property $ \(x :: Double) (y :: Double) ->
        let lit v = "(" <> floatLiteral v <> ")"
        in runLines
             [ "Set.toList (Set.fromList [" <> lit x <> ", " <> lit y <> "])"
                 <> " == (if " <> lit y <> " < " <> lit x
                 <> " then [" <> lit y <> ", " <> lit x <> "]"
                 <> " else if " <> lit x <> " == " <> lit y
                 <> " then [" <> lit x <> "]"
                 <> " else [" <> lit x <> ", " <> lit y <> "])" ]
             === ["true : Bool"]
    -- ANCHOR_END: nanKeys

    -- ANCHOR: usualTypes
    -- 「どの型にもなれる数のある値には、ふつうの型（Int）を添える。
    --   関数は1行のまま」
    it "shows the usual type of a value with numbers of any type" $
      runLines
        [ "treasures = {[(2, 0)]: \"herb\", [(1, 1)]: \"elixir\"}"
        , ":type treasures"
        , "double x = x * 2"
        , "names = {1: \"one\"}"
        , "words = {\"a\": \"b\"}" ]
        `shouldBe`
          [ "treasures : (Number a, KeyComparable a, Number b, \
            \KeyComparable b) => Map (a, b) String\n\
            \  -- usually : Map (Int, Int) String"
          , "(Number a, KeyComparable a, Number b, KeyComparable b) \
            \=> Map (a, b) String\n  -- usually : Map (Int, Int) String"
          , "double : Number a => a -> a"
          , "names : (Number a, KeyComparable a) => Map a String\n\
            \  -- usually : Map Int String"
          , "words : Map String String" ]
    -- ANCHOR_END: usualTypes

    -- ANCHOR: braceError
    -- 「どちらでもないときは、: と = のどちらが来るはずかを言う」
    it "says : or = is needed after a name in { }" $
      runLines ["{name}"]
        `shouldBe`
          [ "<repl>:1:6:\n  |\n1 | {name}\n  |      ^\nunexpected '}'\n\
            \expecting ':' (a Map) or '=' (a record)\n" ]
    -- ANCHOR_END: braceError

    -- 「レコードと Map の { } は、= と : で見分ける」
    it "tells a record from a map by = and :" $
      runLines
        [ "{ name = 1 }", "{ name: 1 }", "{}", "r = { name = 1 }"
        , "{ r | name = 2 }" ]
        `shouldBe`
          [ "{ name = 1 } : { name : Int }"
          , "{\"name\": 1} : Map String Int"
          , "{} : KeyComparable a => Map a b"
          , "r : Number a => { name : a }\n  -- usually : { name : Int }"
          , "{ name = 2 } : { name : Int }" ]

  -- 「JSON」
  describe "JSON" $ do
    -- ANCHOR: preludeCons
    -- 「Haskell が作る構成子の番号は、プレリュードの宣言と同じ」
    it "numbers the constructors as the prelude declares them" $
      [ (c, conIndex <$> Map.lookup c table)
      | let table = Map.unions
              [ constructors (moduleData m)
              | m <- Map.elems (sessionModules preludeSession) ]
      , (c, _) <- preludeConstructors ]
        `shouldBe` [ (c, Just i) | (c, i) <- preludeConstructors ]
    -- ANCHOR_END: preludeCons

    -- ANCHOR: parseJson
    -- 「JSON を読み、Lam の Json にする」
    it "reads JSON into Lam's Json" $
      runLines
        [ "Json.parse \"{\\\"name\\\": \\\"Lam\\\", "
            <> "\\\"tags\\\": [true, null]}\""
        , "Json.parse \"oops\""
        , "Json.parse \"[1] x\""
        , ":type Json.parse" ]
        `shouldBe`
          [ "Ok (JObj {\"name\": JStr \"Lam\", "
              <> "\"tags\": JArr [JBool true, JNull]}) : Result String Json"
          , "Err \"Failed reading: not a valid json value\""
              <> " : Result String Json"
          , "Err \"Failed reading: extra text after the JSON value\""
              <> " : Result String Json"
          , "String -> Result String Json" ]

    -- 「同じキーが2つあれば、後のほうが勝つ（Lam の { } と同じ）」
    it "keeps the last of the same keys, like { } in Lam" $
      runLines
        [ "Json.parse \"{\\\"a\\\": 1, \\\"a\\\": 2}\""
        , "{\"a\": JInt 1, \"a\": JInt 2}" ]
        `shouldBe`
          [ "Ok (JObj {\"a\": JInt 2}) : Result String Json"
          , "{\"a\": JInt 2} : Map String Json" ]
    -- ANCHOR_END: parseJson

    -- ANCHOR: numbers
    -- 「数は、整数なら JInt、小数なら JDec。どちらもぴったり」
    it "reads whole numbers as JInt, and fractions as JDec, exactly" $
      runLines
        [ "Json.parse \"[42, 19.99, -0.5, 1.0, 1e3, 12345678901234567890]\""
        , "Json.show (JArr [Json.mkDec 1999 2, Json.mkDec (0 - 5) 1, JInt 1000])" ]
        `shouldBe`
          [ "Ok (JArr [JInt 42, JDec 1999 2, JDec (-5) 1, JInt 1, JInt 1000, "
              <> "JInt 12345678901234567890]) : Result String Json"
          , "\"[19.99,-0.5,1000]\" : String" ]

    -- 「とても大きな指数でも、桁を並べない（2秒で終わる）」
    it "does not spell out the digits of a very large exponent" $ do
      let lines' = runLines
            [ "Json.parse \"1e1000000000\""
            , "Json.show (Json.mkDec 1 (0 - 1000000000))" ]
      done <- timeout 2000000 (E.evaluate (sum (map T.length lines')))
      fmap (const lines') done `shouldBe` Just
        [ "Ok (JDec 1 (-1000000000)) : Result String Json"
        , "\"1.0e1000000000\" : String" ]
    -- ANCHOR_END: numbers

    -- ANCHOR: floatJson
    -- 「JSON の数と Float を行き来する」
    it "turns JSON numbers into Floats, and back" $
      runLines
        [ "Json.toFloat (Json.mkDec 1999 2)", "Json.toFloat (JInt 3)"
        , "Json.toFloat (JStr \"3\")"
        , "Json.fromFloat 0.1", "Json.fromFloat 2.0"
        , "Json.fromFloat (0 / 0)", "Json.fromFloat (1 / 0)"
        , "Json.show (Json.fromFloat (0.1 + 0.2))" ]
        `shouldBe`
          [ "Just 19.99 : Maybe Float", "Just 3 : Maybe Float"
          , "Nothing : Maybe Float"
          , "JDec 1 1 : Json", "JInt 2 : Json"
          , "JNull : Json", "JNull : Json"
          , "\"0.30000000000000004\" : String" ]

    -- 「Json.fromFloat を Json.toFloat で戻すと、同じ Float」
    it "turns a Float into JSON and back" $
      property $ \x ->
        runLines ["Json.toFloat (Json.fromFloat (" <> floatLiteral x <> "))"]
          === ["Just " <> (if x < 0 then "(" <> showFloat x <> ")"
                           else showFloat x) <> " : Maybe Float"]
    -- ANCHOR_END: floatJson

    -- ANCHOR: mkDec
    -- 「Json.mkDec は、Json.parse と同じ形の小数を作る」
    it "makes a number in the same shape as Json.parse" $
      runLines
        [ "Json.decimal (Json.mkDec 19990 3)"
        , "Json.mkDec 19990 3", "Json.mkDec 1900 2", "Json.mkDec 0 5"
        , "Json.mkDec 1999 2 == Json.mkDec 19990 3" ]
        `shouldBe`
          [ "Just (1999, 2) : Maybe (Int, Int)"
          , "JDec 1999 2 : Json", "JInt 19 : Json", "JInt 0 : Json"
          , "true : Bool" ]

    -- 「どんな c と d でも、Haskell が書いた数 c × 10^(-d) を読んだものと同じ」
    -- （JDec は隠したので、Lam では書けない）
    it "makes the same Json as reading the number c * 10^(-d)" $
      property $ forAll ((,) <$> arbitrary <*> choose (-6, 8)) $
        \(c, d) ->
          let lam n = if n < 0 then "(0 - " <> T.pack (show (abs n)) <> ")"
                               else T.pack (show (n :: Integer))
              written = T.pack (show (scientific c (negate d)))
          in runLines
               [ "Json.mkDec " <> lam c <> " " <> lam (toInteger d)
                   <> " == (case Json.parse \"" <> written
                   <> "\" of | Ok j -> j | Err _ -> JNull)" ]
               === ["true : Bool"]
    -- ANCHOR_END: mkDec

    -- ANCHOR: roundTrip
    -- 「aeson の値を Lam の Json にして戻すと、もとの値」
    it "turns aeson's values into Lam's Json and back" $
      property $ forAll genAeson $ \j ->
        toAeson (fromAeson j) == Right j
    -- ANCHOR_END: roundTrip

    -- ANCHOR: jsonTools
    -- 「奥の値を、道すじで取り出す」
    it "takes a value from deep inside, by its path" $
      runLines
        [ "let doc = JObj {\"user\": "
            <> "JObj {\"name\": JStr \"Lam\", \"age\": JInt 3}}"
        , "Json.at [\"user\", \"name\"] doc"
        , "Maybe.andThen Json.int (Json.at [\"user\", \"age\"] doc)"
        , "Maybe.andThen Json.int (Json.at [\"user\", \"name\"] doc)"
        , "Json.at [\"user\", \"email\"] doc"
        , "Json.at [] (JInt 1)" ]
        `shouldBe`
          [ "doc : Json"
          , "Just (JStr \"Lam\") : Maybe Json"
          , "Just 3 : Maybe Int"
          , "Nothing : Maybe Int"
          , "Nothing : Maybe Json"
          , "Just (JInt 1) : Maybe Json" ]
    -- ANCHOR_END: jsonTools

  -- 「道具箱」
  describe "toolbox" $ do
    -- ANCHOR: toolboxStrings
    -- 「文字列の道具」
    it "trims, splits and replaces strings" $
      runLines
        [ "String.trim \"  Lam  \""
        , "String.split \",\" \"a,,b\""
        , "\" TS , ts@lam.test \" |> String.split \",\" |> map String.trim"
        , "String.startsWith \"#\" \"# note\""
        , "String.contains \"@\" \"ruby-at-lam.test\""
        , "String.replace \"-at-\" \"@\" \"ruby-at-lam.test\""
        , "String.repeat 3 \"ab\""
        , "String.split \"\" \"abc\"" ]
        `shouldBe`
          [ "\"Lam\" : String"
          , "[\"a\", \"\", \"b\"] : [String]"
          , "[\"TS\", \"ts@lam.test\"] : [String]"
          , "true : Bool"
          , "false : Bool"
          , "\"ruby@lam.test\" : String"
          , "\"ababab\" : String"
          , "[\"a\", \"b\", \"c\"] : [String]" ]
    -- 「区切りが空でなければ、Haskell と同じ。区切りでつなぎ直すと元に戻る」
    it "splits as Haskell's splitOn does, and join undoes it" $
      property $ forAll ((,) <$> elements ["", ",", ", ", "ab"] <*> genText) $
        \(sep, text) ->
          runLines [ "String.split " <> renderString sep <> " "
                       <> renderString text ]
          === [ "[" <> T.intercalate ", "
                         (map renderString (splitOn sep text))
                  <> "] : [String]" ]
          .&&. (T.null sep || T.intercalate sep (splitOn sep text) == text)
          .&&. (T.null sep || splitOn sep text == T.splitOn sep text)
    -- ANCHOR_END: toolboxStrings

    -- ANCHOR: toolboxSequence
    -- 「reverse・take・slice は、文字列にもリストにも」
    it "reverses, takes and slices strings and lists alike" $
      runLines
        [ "reverse \"abc\""
        , "reverse [1, 2, 3]"
        , "take 3 \"Haskell\""
        , "drop 1 [1, 2, 3]"
        , "slice 1 (-1) \"Lam!\""
        , "slice 1 (-1) [1, 2, 3, 4]"
        , "let palindrome s = s == reverse s"
        , "map palindrome [\"level\", \"lam\"]"
        , "palindrome [1, 2, 1]"
        , "reverse (Just 1)" ]
        `shouldBe`
          [ "\"cba\" : String"
          , "[3, 2, 1] : [Int]"
          , "\"Has\" : String"
          , "[2, 3] : [Int]"
          , "\"am\" : String"
          , "[2, 3] : [Int]"
          , "palindrome : (Comparable a, Sequence a) => a -> Bool"
          , "[true, false] : [Bool]"
          , "true : Bool"
          , "type error: Maybe a is not in the class Sequence" ]
    -- 「slice は Python の s[i:j] と同じ（負の数は後ろから）」
    it "slices as Python's s[i:j] does" $
      property $ \(i, j) -> forAll genText $ \text ->
        let len = T.length text
            place k = max 0 (min len (if k < 0 then len + k else k))
            python = T.drop (place i) (T.take (place j) text)
        in runLines [ "slice " <> num (toInteger (i :: Int)) <> " "
                        <> num (toInteger (j :: Int)) <> " "
                        <> renderString text ]
           === [ renderString python <> " : String" ]
    -- 「文字列の reverse は Haskell と同じ」
    it "reverses a string as Haskell does" $
      property $ forAll genText $ \text ->
        runLines ["reverse " <> renderString text]
          === [renderString (T.reverse text) <> " : String"]
    -- ANCHOR_END: toolboxSequence

    -- ANCHOR: toolboxListMax
    -- 「リストの版：空の maximum は Nothing」
    it "gives Nothing for the maximum of an empty list" $
      runLines [ "List.maximum [95, 88, 79]", "List.maximum []"
               , ":type List.maximum" ]
        `shouldBe` [ "Just 95 : Maybe Int"
                   , "Nothing : Comparable a => Maybe a"
                   , "Comparable a => [a] -> Maybe a" ]
    -- ANCHOR_END: toolboxListMax

    -- ANCHOR: toolboxKinds
    -- 「f a の型を書き、穴のある型だけを instance にする」
    it "writes f a in types, and checks the holes of an instance" $
      runLines
        [ ":type toList"
        , ":type foldr"
        , "instance Foldable Int where foldr f z x = z"
        , "class Box f where unbox : f a -> a"
        , "instance Box Maybe where unbox m = m ?: unbox m"
        , "instance Box (Maybe a) where unbox m = m"
        , "class Size a where size : a -> Int"
        , "instance Size Maybe where size m = 1"
        , "instance Size (Maybe a) where size m = 1"
        , "instance Size (a, b) where size p = 2" ]
        `shouldBe`
          [ "Foldable f => f a -> [a]"
          , "Foldable f => (a -> b -> b) -> b -> f a -> b"
          , "in the instance Foldable Int: type error: Foldable is for a type"
              <> " with a hole (one that takes 1 more type, like List or"
              <> " Maybe), not Int"
          , "unbox : Box f => f a -> a"
          , "instance Box Maybe"
          , "in the instance Box (Maybe a): type error: Box is for a type"
              <> " with a hole (one that takes 1 more type, like List or"
              <> " Maybe), not Maybe a"
          , "size : Size a => a -> Int"
          , "in the instance Size Maybe: type error: Size is for a type"
              <> " with no hole (one that takes 0 more types, like Int or"
              <> " (Maybe a)), not Maybe"
          , "instance Size (Maybe a)"
          , "instance Size (a, b)" ]
    -- ANCHOR_END: toolboxKinds

    -- ANCHOR: toolboxFoldable
    -- 「Foldable：リストにも Map にも Maybe にも Set にも」
    it "folds lists, maps, Maybes and sets alike" $
      runLines
        [ ":type any"
        , "any even [1, 3, 4]"
        , "sum {python: 82, ts: 75}"
        , "maximum {python: 82, ts: 75}"
        , "maximum []"
        , "sum (Just 3)"
        , "elem \"Lam\" (Just \"Lam\")"
        , "toList {b: 2, a: 1}"
        , "sum (Set.fromList [3, 1, 3])"
        , "sum 5"
        , "any even \"abc\"" ]
        `shouldBe`
          [ "Foldable f => (a -> Bool) -> f a -> Bool"
          , "true : Bool"
          , "157 : Int"
          , "Just 82 : Maybe Int"
          , "Nothing : Comparable a => Maybe a"
          , "3 : Int"
          , "true : Bool"
          , "[1, 2] : [Int]"
          , "4 : Int"
          , "type error: the types do not fit: Int where f a is needed"
          , "type error: the types do not fit: String where f Int is needed" ]
    -- 「maximum は、Haskell の maximum と同じ。空なら Nothing」
    it "finds the same maximum as Haskell, and Nothing for []" $
      property $ \xs ->
        map valuePart
          (runLines ["maximum " <> lamList xs, "minimum " <> lamList xs])
          === map valuePart
                [ lamMaybe (if null xs then Nothing else Just (maximum xs))
                , lamMaybe (if null xs then Nothing else Just (minimum xs)) ]
    -- 「Map の値は、キーの順にたたむ（引き算で順番まで確かめる）」
    it "folds the values of a map in the order of the keys" $
      property $ \kvs ->
        let m = Map.fromList [ (k :: Int, v :: Integer) | (k, v) <- kvs ]
            lit = "(Map.fromList [" <> T.intercalate ", "
                    [ "(" <> num (toInteger k) <> ", " <> num v <> ")"
                    | (k, v) <- Map.toList m ] <> "])"
        in map valuePart
             (runLines [ "sum " <> lit, "foldr (\\x acc -> x - acc) 0 " <> lit ])
           === [ T.pack (show (sum m)), T.pack (show (foldr (-) 0 m)) ]
    -- ANCHOR_END: toolboxFoldable

    -- ANCHOR: toolboxPending
    -- 「f がわからないうちの条件は待ち、最後まで決まらなければリストに」
    it "waits for f, and makes it a list when it is never known" $
      runLines
        [ "let total t = sum t"
        , "let average xs = sum xs / toFloat (length xs)"
        , "average [80, 90]"
        , "let grow t = map (\\x -> x + 1) t ++ [0]"
        , "(\\t -> length (map (\\x -> x) t)) (Just 1)" ]
        `shouldBe`
          [ "total : (Foldable f, Number a) => f a -> a"
          , "average : [Float] -> Float"
          , "85 : Float"
          , "grow : Number a => [a] -> [a]"
          , "type error: Maybe a is not in the class HasLength" ]
    -- ANCHOR_END: toolboxPending

    -- ANCHOR: toolboxFunctor
    -- 「Functor：map はリストにも Maybe にも Map にも Result にも」
    it "maps lists, Maybes, maps and Results alike" $
      runLines
        [ ":type map"
        , "map (\\n -> n * 2) [1, 2]"
        , "map (\\n -> n * 2) (Just 4)"
        , "map (\\n -> n * 2) {a: 1, b: 2}"
        , "map (\\n -> n * 2) (Ok 3)"
        , "map (\\n -> n * 2) (Err \"no\")"
        , "map toUpper \"abc\""
        , ":dicts map (\\n -> n * 2) (Just 4)" ]
        `shouldBe`
          [ "Functor f => (a -> b) -> f a -> f b"
          , "[2, 4] : [Int]"
          , "Just 8 : Maybe Int"
          , "{\"a\": 2, \"b\": 4} : Map String Int"
          , "Ok 6 : Result a Int"
          , "Err \"no\" : Result String Int"
          , "type error: the types do not fit: String where f String"
              <> " is needed"
          , "map $Functor$Maybe (\\n -> n * 2) (Just 4)" ]
    -- ANCHOR_END: toolboxFunctor

    -- ANCHOR: toolboxFallible
    -- 「?: は Fallible の toMaybe で開く。右は要るときだけ計算する」
    it "opens ?: with toMaybe, and computes the right only when needed" $
      runLines
        [ ":type \\m -> m ?: 0"
        , "Just 3 ?: (1 // 0)"
        , "Err \"no\" ?: 4"
        , "3 ?: 0"
        , "[1] ?: 0"
        , ":dicts Just 3 ?: 0" ]
        `shouldBe`
          [ "(Fallible f, Number a) => f a -> a"
          , "3 : Int"
          , "4 : Int"
          , "type error: ?: needs a Maybe or a Result on its left, but got"
              <> " a number (only a Maybe or a Result can be missing its"
              <> " value)"
          , "type error: ?: needs a Maybe or a Result on its left, but got"
              <> " [a] (only a Maybe or a Result can be missing its value)"
          , "case toMaybe $Fallible$Maybe (Just 3) of | Just $x -> $x"
              <> " | Nothing -> 0" ]
    -- ANCHOR_END: toolboxFallible

    -- ANCHOR: toolboxOwn
    -- 「自分の data の型も、Foldable・Functor・Fallible に入る」
    it "puts our own data types into the classes of the toolbox" $ do
      team <- replAfter "examples/stdlib/team.lam"
        [ "maximum (Team 1 3 2)", ":dicts sum (Team 1 2 3)" ]
      skipped <- replAfter "examples/stdlib/skipped.lam"
        [ ":type toMaybe", "Answered \"a\" ?: \"none\"" ]
      (team, skipped) `shouldBe`
        ( [ "Just 3 : Maybe Int", "sum $Foldable$Team (Team 1 2 3)" ]
        , [ "Fallible f => f a -> Maybe a", "\"a\" : String" ] )
    -- ANCHOR_END: toolboxOwn

    -- ANCHOR: toolboxLists
    -- 「リストの道具は、Haskell の zip・nub・partition・sortOn と同じ」
    it "zips, numbers, splits, sorts and makes unique as Haskell does" $
      property $ \xs ys ->
        let shown zs = "[" <> T.intercalate ", " (map (T.pack . show) zs)
                         <> "]"
        in map valuePart
             (runLines [ "zip " <> lamList xs <> " " <> lamList ys
                       , "unique " <> lamList xs
                       , "partition even " <> lamList xs
                       , "map fst (indexed " <> lamList xs <> ")"
                       , "sortOn negate " <> lamList xs ])
           === [ "[" <> T.intercalate ", "
                   [ "(" <> T.pack (show a) <> ", " <> T.pack (show b) <> ")"
                   | (a, b) <- zip xs ys ] <> "]"
               , shown (nub xs)
               , "(" <> shown (filter even xs) <> ", "
                   <> shown (filter odd xs) <> ")"
               , shown (map toInteger [0 .. length xs - 1])
               , shown (sortOn negate xs) ]
    -- 「sortOn は、同じキーの2つの順を変えない」
    it "keeps the order of the items with the same key" $
      property $ \(kvs :: [(Integer, Integer)]) ->
        let lit = "[" <> T.intercalate ", "
                    [ "(" <> num k <> ", " <> num v <> ")" | (k, v) <- kvs ]
                    <> "]"
            shownPairs ps = "[" <> T.intercalate ", "
              [ "(" <> T.pack (show k) <> ", " <> T.pack (show v) <> ")"
              | (k, v) <- ps ] <> "]"
        in map valuePart (runLines ["sortOn fst " <> lit])
             === [shownPairs (sortOn fst kvs)]
    -- ANCHOR_END: toolboxLists

    -- ANCHOR: toolboxMaps
    -- 「Map の道具は、Haskell の Map と同じ（unionWith は引数の順も）」
    it "unions, filters and updates maps as Haskell's Map does" $
      property $ \as bs k ->
        let a = Map.fromList [ (key :: Int, v :: Integer) | (key, v) <- as ]
            b = Map.fromList [ (key, v) | (key, v) <- bs ]
            lit m = "(Map.fromList [" <> T.intercalate ", "
                      [ "(" <> num (toInteger x) <> ", " <> num v <> ")"
                      | (x, v) <- Map.toList m ] <> "])"
            shown m = "{" <> T.intercalate ", "
                        [ T.pack (show x) <> ": " <> T.pack (show v)
                        | (x, v) <- Map.toList m ] <> "}"
            bump n = case n of
              Just v | v > 0 -> Just (v - 1)
              _              -> Nothing
        in map valuePart (runLines
             [ "Map.unionWith (\\x y -> x - y) " <> lit a <> " " <> lit b
             , "Map.filter (\\v -> v > 0) " <> lit a
             , "Map.update " <> num (toInteger (k :: Int))
                 <> " (\\n -> case n of | Just v -> if v > 0"
                 <> " then Just (v - 1) else Nothing | Nothing -> Nothing) "
                 <> lit a
             , "Map.union " <> lit a <> " " <> lit b
             , "Map.map (\\v -> v * 2) " <> lit a ])
           === map shown [ Map.unionWith (-) a b, Map.filter (> 0) a
                         , Map.alter bump k a, Map.union a b
                         , Map.map (* 2) a ]
    -- ANCHOR_END: toolboxMaps

    -- ANCHOR: toolboxPretty
    -- 「Json.pretty は、何行かに分けて2つずつ字下げする。読み戻すと同じ」
    it "writes JSON on many lines, and parse reads it back" $
      runLines
        [ "Json.pretty (JArr [JInt 1, JStr \"a\"])"
        , "Json.parse (Json.pretty (JObj {b: JNull, a: JBool true}))" ]
        `shouldBe`
          [ "\"[\\n  1,\\n  \\\"a\\\"\\n]\" : String"
          , "Ok (JObj {\"a\": JBool true, \"b\": JNull})"
              <> " : Result String Json" ]
    -- ANCHOR_END: toolboxPretty

    -- ANCHOR: quizRepl
    -- 「本文の、例を :load してからのやり取り」
    it "answers the REPL lines after loading the examples" $ do
      signup <- replAfter "examples/stdlib/signup.lam"
        [ "cells \"  Python , python@lam.test , Python\""
        , "fixMail \"ruby-at-lam.test\"", "map .mail entries" ]
      quiz <- replAfter "examples/stdlib/quiz.lam"
        [ "best [95, 88, 79]", "best []", "totals", "bestTotal", "allPoints"
        , "passedNames", "notYet", "map best book", "notice" ]
      tally <- replAfter "examples/stdlib/tally.lam" ["map showCounts days"]
      (signup, quiz, tally) `shouldBe`
        ( [ "[\"Python\", \"python@lam.test\", \"Python\"] : [String]"
          , "\"ruby@lam.test\" : String"
          , "[\"python@lam.test\", \"ts@lam.test\", \"rust@lam.test\","
              <> " \"ruby@lam.test\"] : [String]" ]
        , [ "\"95\" : String", "\"-\" : String"
          , "{\"Erlang\": 156, \"Python\": 247, \"Ruby\": 0, \"Rust\": 262,"
              <> " \"TS\": 159} : Map String Int"
          , "262 : Int", "824 : Int"
          , "[\"Python\", \"Rust\", \"TS\"] : [String]"
          , "[\"Erlang\", \"Ruby\"] : [String]"
          , "{\"Erlang\": \"61\", \"Python\": \"90\", \"Ruby\": \"-\","
              <> " \"Rust\": \"95\", \"TS\": \"91\"} : Map String String"
          , "\"{\\n  \\\"Erlang\\\": 156,\\n  \\\"Python\\\": 247,\\n  \\\"Ruby"
              <> "\\\": 0,\\n  \\\"Rust\\\": 262,\\n  \\\"TS\\\": 159\\n}\" : String" ]
        , [ "[\"Haskell 1, Lam 2, Python 1\", \"Go 1, Haskell 2, Lam 1,"
              <> " Rust 1\"] : [String]" ] )
    -- ANCHOR_END: quizRepl

  -- 「値を埋めこむ書式」
  describe "f-strings" $ do
    -- ANCHOR: formatSugar
    -- 「f"..." は、++ と関数の呼び出しに開く」
    -- (The names it calls are hidden ones, like $fmt.display: see the
    -- tests of the modules. Here, without $fmt.)
    -- （呼ぶ名前は $fmt.display のような隠した名前。ここでは $fmt. を外す）
    it "opens f\"...\" into ++ and calls" $
      map (fmap (T.replace "$fmt." "" . render) . parseExpr)
        [ "f\"Hello, {name}!\""
        , "f\"{n:>8,} {n:05}\""
        , "f\"{x:.1} {x:>9,.2}\""
        , "f\"{x=} {x = :^6}\""
        , "f\"{{}} {f\"{x}\"}\""
        , "f\"\"" ]
        `shouldBe` map Right
          [ "\"Hello, \" ++ display name ++ \"!\""
          , "padLeft 8 (groupDigits (showNumber n)) ++ \" \""
              <> " ++ zeroPad 5 (showNumber n)"
          , "Float.fixed 1 x ++ \" \" ++ padLeft 9 (groupDigits"
              <> " (Float.fixed 2 x))"
          , "\"x=\" ++ show x ++ \" x = \" ++ center 6 (display x)"
          , "\"{} \" ++ display (display x)"
          , "\"\"" ]
    -- ANCHOR_END: formatSugar

    -- ANCHOR: formatValues
    -- 「値を埋めこみ、桁をそろえる（Python と同じ答え）」
    it "puts values in, and lines them up like Python" $
      runLines
        [ "name = \"Lam\""
        , "f\"[{name:<6}] [{name:>6}] [{name:^6}] [{name:^5}]\""
        , "f\"{1234567:,} {-1234:,} {42:05} {-42:05} {123456:03}\""
        , "f\"{name=} {[name]} {Just (1, true)} {{\""
        , "f\"{name:>2}|{\"日本\":>4}|\""
        , "f\"{2.5:.2} {-0.5:.1} {1234567.891:,.2} {3.14159:08.3}\"" ]
        `shouldBe`
          [ "name : String"
          , "\"[Lam   ] [   Lam] [ Lam  ] [ Lam ]\" : String"
          , "\"1,234,567 -1,234 00042 -0042 123456\" : String"
          , "\"name=\\\"Lam\\\" [\\\"Lam\\\"] Just (1, true) {\" : String"
          , "\"Lam|  日本|\" : String"
          , "\"2.50 -0.5 1,234,567.89 0003.142\" : String" ]
    -- ANCHOR_END: formatValues

    -- ANCHOR: formatErrors
    -- 「書式のまちがいは、動かす前にパーサーが見つける」
    it "finds the mistakes of a format before running" $
      map (fmap (const ()) . parseExpr)
        [ "f\"{x:>}\"", "f\"{x:8}\"", "f\"{x:>05}\"", "f\"{x:05,}\""
        , "f\"{x:>8d}\"", "f\"{x:.}\"", "f\"{x:.1f}\"", "f\"{x : xs}\""
        , "f\"{\\y -> y}\"", "f\"a } b\"" ]
        `shouldSatisfy` all isLeft
    -- ANCHOR_END: formatErrors

    -- 「エラーの文が、まちがいの場所を言う」
    it "says what is wrong with a format, and where" $
      runLines ["f\"{x:>}\"", "f\"{x:8}\"", "f\"{x:.2f}\"", "f\"a } b\""]
        `shouldBe`
          [ "<repl>:1:7:\n  |\n1 | f\"{x:>}\"\n  |       ^\n\
            \a width must follow < > or ^\n"
          , "<repl>:1:6:\n  |\n1 | f\"{x:8}\"\n  |      ^\n\
            \a width needs < > or ^ before it (or 0, for a number)\n"
          , "<repl>:1:8:\n  |\n1 | f\"{x:.2f}\"\n  |        ^\n\
            \a format has no type letter like f (write {x:.1}, not {x:.1f})\n"
          , "<repl>:1:5:\n  |\n1 | f\"a } b\"\n  |     ^\n\
            \a single } must be written }}\n" ]

    -- ANCHOR: formatTypes
    -- 「埋めこめない値と、数にしか使えない書式は、型のエラー」
    it "gives a type error for values that cannot be shown" $
      runLines
        [ ":type show"
        , ":type display"
        , "f\"{\\n -> n}\""
        , "f\"{(\\n -> n)}\""
        , "f\"{\"Lam\":,}\""
        , "f\"{\"Lam\":.1}\""
        , "data Handler = Button (Int -> Int)"
        , "show (Button (\\n -> n))"
        , "instance Display Handler"
        , "data Box a = Box a"
        , "f\"{Box 1}\""
        , "show (Box {a: [1]})" ]
        `shouldBe`
          [ "Showable a => a -> String"
          , "Display a => a -> String"
          , "<repl>:1:4:\n  |\n1 | f\"{\\n -> n}\"\n  |    ^\n\
            \inside { }, put \\, let, if and case in ( )\n"
          , "type error: a -> a is not in the class Display"
          , "type error: the types do not fit: a number where String \
            \is needed"
          , "type error: the types do not fit: a number where String \
            \is needed"
          , "Button : (Int -> Int) -> Handler"
          , "type error: Handler is not in the class Showable \
            \(Button holds Int -> Int)"
          , "in the instance Display Handler: type error: Handler is not \
            \in the class Showable (Button holds Int -> Int)"
          , "Box : a -> Box a"
          , "type error: Box a is not in the class Display"
          , "\"Box {\\\"a\\\": [1]}\" : String" ]
    -- ANCHOR_END: formatTypes

    -- ANCHOR: formatDisplay
    -- 「Display：自分の型は1行で入れるか、見せ方を自分で決める」
    it "shows our own types and JSON with Display" $
      runLines
        [ "display \"Lam\"", "show \"Lam\""
        , "f\"{Json.mkDec 1999 2} {JArr [JInt 1, JNull]}\""
        , "data Money = Yen Int"
        , "instance Display Money"
        , "f\"{Yen 300}\""
        , "data Tag = Tag String"
        , "instance Display Tag where display (Tag s) = \"#\" ++ s"
        , "f\"{Tag \"lam\"} {[Tag \"a\"]}\"" ]
        `shouldBe`
          [ "\"Lam\" : String", "\"\\\"Lam\\\"\" : String"
          , "\"19.99 [1,null]\" : String"
          , "Yen : Int -> Money"
          , "instance Display Money"
          , "\"Yen 300\" : String"
          , "Tag : String -> Tag"
          , "instance Display Tag"
          , "\"#lam [Tag \\\"a\\\"]\" : String" ]
    -- ANCHOR_END: formatDisplay

    -- 「f と文字列のあいだに空白があれば、いままでどおり関数の呼び出し」
    it "still calls a function f when a space comes before the string" $
      runLines ["f s = s ++ \"!\"", "f \"hi\"", "f\"hi\""]
        `shouldBe`
          ["f : String -> String", "\"hi!\" : String", "\"hi\" : String"]

    -- ANCHOR: formatPrint
    -- 「:print は、文字列をそのまま書く。レシートも、そろって見える」
    it "writes the receipt with :print" $ do
      out <- replAfter "examples/format/receipt.lam"
        [":print receipt", ":print 3"]
      out `shouldBe`
        [ T.intercalate "\n"
            [ "             RECEIPT              "
            , "sticker     4 x    300 =     1,200"
            , "mug         1 x  1,200 =     1,200"
            , "tshirt      3 x  2,500 =     7,500"
            , "----------------------------------"
            , "total                        9,900"
            , "per item                   1,237.5" ]
        , "type error: :print writes a String or a [String], not Int" ]
    -- ANCHOR_END: formatPrint

    -- ANCHOR: padProps
    -- 「そろえた文字は、幅より短くならず、もとの文字を右・左・真ん中に持つ」
    it "pads to the width, keeping the text on its side" $
      forAll ((,) <$> choose (0, 12) <*> genText) $ \(w, s) ->
        let n       = max (fromIntegral w) (T.length s)
            blank k = T.replicate k " "
            d       = n - T.length s
        in  padLeft w s  === blank d <> s
       .&&. padRight w s === s <> blank d
       .&&. center w s   === blank (d `div` 2) <> s <> blank (d - d `div` 2)

    -- 「カンマを外すと、もとの数。0 で埋めた数を読むと、もとの数」
    it "groups digits and pads with zeros without changing the number" $
      forAll ((,) <$> arbitrary <*> choose (0, 12)) $ \(n, w) ->
        let text    = T.pack (show (n :: Integer))
            grouped = groupDigits text
            parts   = T.splitOn "," (T.dropWhile (== '-') grouped)
            padded  = zeroPad w text
        in  read (T.unpack (T.filter (/= ',') grouped)) === n
       .&&. all ((== 3) . T.length) (drop 1 parts)
       .&&. all ((`elem` [1, 2, 3]) . T.length) (take 1 parts)
       .&&. read (T.unpack (T.filter (/= '-') padded)) === abs n
       .&&. T.isPrefixOf "-" padded === (n < 0)
       .&&. T.length padded === max (fromIntegral w) (T.length text)
    -- ANCHOR_END: padProps


  -- 「モジュール」
  describe "modules" $ do
    -- ANCHOR: moduleGolden
    mapM_ (golden . ("modules/" <>))
      [ "receipt/today", "tools/week", "price/menu", "price/broken"
      , "shapes/gallery", "shapes/both", "clash/report", "clash/fixed"
      , "ring/shop" ]
    -- ANCHOR_END: moduleGolden

    -- ANCHOR: parseModule
    -- 「module と import の行を読む」
    it "reads the module line and the import lines" $
      fmap (\(Program h is _ _) -> (h, is))
        (runLamParser ops program "m.lam" (T.unlines
          [ "module Shapes exposing (Shape (..), area, Describe (describe))"
          , "import Json"
          , "import Json as J (get, Json (JInt, JStr))" ]))
        `shouldBe` Right
          ( Just (Header (Loc "m.lam" 1 1) "Shapes" (ExposeSome
              [ ExposeType "Shape" AllConstructors, ExposeValue "area"
              , ExposeType "Describe" (TheseConstructors ["describe"]) ]))
          , [ Import (Loc "m.lam" 2 1) "Json" Nothing Nothing
            , Import (Loc "m.lam" 3 1) "Json" (Just "J")
                (Just (ExposeSome
                  [ ExposeValue "get"
                  , ExposeType "Json" (TheseConstructors ["JInt", "JStr"])
                  ])) ] )
    -- ANCHOR_END: parseModule

    -- ANCHOR: importNames
    -- 「import した名前の付き方：前にモジュールの名前、as なら別名だけ、
    --   かっこの一覧なら名前なしでも」
    it "names what it imports: with the module, the alias, or bare" $ do
      withShapes "import Shapes\nShapes.area (Shapes.square 3)"
        `shouldBe` Right "9 : Int"
      withShapes "import Shapes as S\nS.area (S.Circle 2)"
        `shouldBe` Right "12 : Int"
      -- Unlike Haskell, as takes the place of the name: Shapes. is gone.
      -- Haskell とちがい、as は名前の代わり。Shapes. は使えない
      withShapes "import Shapes as S\nShapes.area (S.Circle 2)"
        `shouldBe` Left "main.lam:2:1: type error: unknown name: Shapes.area"
      withShapes "import Shapes (area, Shape (..))\narea (Rect 2 5)"
        `shouldBe` Right "10 : Int"
      withShapes "import Shapes (area)\nsquare 3"
        `shouldBe` Left "main.lam:2:1: type error: unknown name: square"
      withShapes (T.unlines [ "import Shapes as S (area)"
                            , "(area (S.Circle 1), S.area (S.square 2))" ])
        `shouldBe` Right "(3, 4) : (Int, Int)"
      -- A ( at column 1 is the expression at the end, not the list.
      -- 1桁目の ( は、一覧ではなく、最後の式
      withShapes "import Shapes\n(Shapes.area (Shapes.square 2), 1)"
        `shouldBe` Right "(4, 1) : (Int, Int)"
    -- ANCHOR_END: importNames

    -- ANCHOR: hiding
    -- 「見せていない名前は、ほかのファイルからは見えない」
    it "keeps the names a module does not expose" $ do
      withShapes "import Shapes\nShapes.secret"
        `shouldBe` Left "type error: unknown name: Shapes.secret"
      withShapes "import Shapes (secret)\n1"
        `shouldBe` Left
          "main.lam:1:1: error: the module Shapes does not expose secret"
      -- A constructor hidden by Json: written bare or with Json.
      -- Json が隠した構成子。名前なしでも、Json. を付けても
      runLines
        [ "JDec 1 2", "Json.JDec 1 2", "case JInt 1 of | JDec c d -> c" ]
        `shouldBe` map (\c -> "type error: " <> c
                          <> " is hidden by the module Json"
                          <> " (it is not in its exposing list)")
          [ "JDec", "Json.JDec", "JDec" ]
      -- The values still show as JDec: only writing it is not allowed.
      -- 値の見せ方は JDec のまま。書くことだけができない
      runLines [ "Json.mkDec 1999 2", "Json.decimal (Json.mkDec 19990 3)" ]
        `shouldBe` [ "JDec 1999 2 : Json"
                   , "Just (1999, 2) : Maybe (Int, Int)" ]
    -- ANCHOR_END: hiding

    -- ANCHOR: patterns
    -- 「別名の付いた構成子も、パターンに書ける」
    it "matches constructors written with the module's name" $ do
      withShapes (T.unlines
        [ "import Shapes as S"
        , "name s = case s of"
        , "  | S.Circle _ -> \"circle\""
        , "  | S.Rect _ _ -> \"rect\""
        , "(name (S.Circle 1), name (S.square 2))" ])
        `shouldBe` Right "(\"circle\", \"rect\") : (String, String)"
      runLines [ "case Json.mkDec 1 1 of | Json.JInt n -> n | _ -> -1" ]
        `shouldBe` [ "-1 : Int" ]
    -- ANCHOR_END: patterns

    -- ANCHOR: classTests
    -- 「クラスとメソッドを見せる。インスタンスは、どこへでも付いていく」
    it "shows a class with its methods, and takes the instances along" $ do
      withShapes "import Shapes\nShapes.describe (Shapes.Circle 2)"
        `shouldBe` Right "\"circle 2\" : String"
      withShapes "import Shapes (Describe (..))\ndescribe (Shapes.Rect 1 2)"
        `shouldBe` Right "\"rect 1 2\" : String"
      -- Our own type in the class of Shapes.
      -- Shapes のクラスに、自分の型を入れる
      withShapes (T.unlines
        [ "import Shapes (Describe (..))"
        , "data Dot = Dot"
        , "instance Describe Dot where"
        , "  describe d = \"a dot\""
        , "describe Dot" ])
        `shouldBe` Right "\"a dot\" : String"
      -- Describe without (..): the class, but not its method.
      -- (..) のない Describe：クラスだけで、メソッドは見えない
      runModules
        [ ("Shapes.lam", T.replace "Describe (..)" "Describe" shapes) ]
        "import Shapes\nShapes.describe (Shapes.Circle 2)"
        `shouldBe` Left
          "main.lam:2:1: type error: Shapes.describe is hidden by the \
          \module Shapes (it is not in its exposing list)"
      -- The instance comes with the type, with no word in the list.
      -- インスタンスは、一覧に何も書かなくても、型と一緒に来る
      withShapes "import Shapes (Shape (..))\nf\"{Circle 2}\""
        `shouldBe` Right "\"(circle 2)\" : String"
    -- ANCHOR_END: classTests

    -- ANCHOR: instanceClash
    -- 「同じインスタンスが2つのモジュールから来たら、エラーにする」
    it "stops when two modules declare the same instance" $ do
      let shown m how = T.unlines
            [ "module " <> m <> " exposing (..)"
            , "import Box (Box (..))"
            , "instance Display Box where"
            , "  display b = " <> how
            , "tag = 1" ]
          go = runModules
            [ ("Box.lam", "module Box exposing (Box (..))\ndata Box = Box")
            , ("A.lam", shown "A" "\"a\""), ("B.lam", shown "B" "\"b\"") ]
      go "import A\nimport B\n1" `shouldBe` Left
        "main.lam:2:1: error: the instance Display Box comes from both \
        \A and B"
      go "import A\nimport Box\nf\"{Box.Box}\""
        `shouldBe` Right "\"a\" : String"
    -- ANCHOR_END: instanceClash

    -- ANCHOR: fmtNames
    -- 「f"..." は隠した名前を呼ぶので、import や自分の定義に左右されない」
    it "calls hidden names from f\"...\", whatever is imported" $ do
      fmap render (parseExpr "f\"{x:>5}\"")
        `shouldBe` Right "$fmt.padLeft 5 ($fmt.display x)"
      -- Our own display, padLeft and show do not change f"...".
      -- 自分の display・padLeft・show は、f"..." を変えない
      runModules []
        "display x = \"own\"\n\
        \padLeft n s = \"own\"\n\
        \show = 0\n\
        \(f\"{12:>4}|{[1]=}\", display 1)"
        `shouldBe` Right "(\"  12|[1]=[1]\", \"own\") : (String, String)"
    -- ANCHOR_END: fmtNames

    -- ANCHOR: standardTests
    -- 「標準のモジュール：別名を付けられ、中の組み込みも見せる」
    it "makes the standard library into modules" $ do
      out <- replImporting []
        [ "import Map as M"
        , "M.insertWith (+) \"a\" 1 (M.fromList [(\"a\", 2)])"
        , "import Float as F (isNaN)"
        , "(F.pow 2.0 3.0, isNaN (0.0 / 0.0))"
        , "Prelude.map (\\x -> x + 1) [1, 2]"
        , "List.sortBy (\\a b -> a > b) [1, 3, 2]" ]
      out `shouldBe`
          [ "", "{\"a\": 3} : Map String Int", ""
          , "(8, true) : (Float, Bool)"
          , "[2, 3] : [Int]", "[3, 2, 1] : [Int]" ]
      Map.keys (sessionModules preludeSession) `shouldBe`
        [ "Actor", "Base64", "Bytes", "Chan", "Cli", "Csv", "Decode", "File"
        , "Float", "Hash"
        , "Http"
        , "IO", "Json", "List", "Map", "Maybe", "Parser", "Path", "Prelude"
        , "Process", "Random", "Regex", "Response", "Result", "Route", "Send"
        , "Set", "Shell", "Sql", "String", "Term", "Time", "Toml", "Uuid" ]
    -- ANCHOR_END: standardTests

    -- ANCHOR: finding
    -- 「探す順：まず隣、それから LAM_PATH のフォルダーを順に」
    it "looks next to the file first, then in LAM_PATH" $ do
      let next  = "module Greet exposing (hi)\nhi = \"next to it\""
          inLib = "module Greet exposing (hi)\nhi = \"in lib\""
          go files = fmap (fromMaybe "" . snd) $ (>>= finalText) $
            runIdentity $
            loadProgram (mapFiles (Map.fromList files) ["lib", "more"])
              preludeSession preludeSession "app/main.lam"
              "import Greet\nGreet.hi"
      go [("app/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Right "\"next to it\" : String"
      go [("more/Greet.lam", next), ("lib/Greet.lam", inLib)]
        `shouldBe` Right "\"in lib\" : String"
      go [] `shouldBe` Left ("app/main.lam:1:1: error: no module Greet "
        <> "(looked for app/Greet.lam, lib/Greet.lam, more/Greet.lam)")
    -- ANCHOR_END: finding

    -- ANCHOR: once
    -- 「2つのファイルが import しても、モジュールのファイルは1度だけ読む」
    it "reads a module file only once, even if two files import it" $ do
      let files = Map.fromList
            [ ("A.lam", "module A exposing (a)\nimport C\na = C.c + 1")
            , ("B.lam", "module B exposing (b)\nimport C\nb = C.c + 2")
            , ("C.lam", "module C exposing (c)\nc = 10") ]
          logging = Files
            { readSource = \path -> do
                modify (path :)
                pure (Map.lookup path files)
            , searchPath = [] }
          (loaded, readLog) = runState
            (loadProgram logging preludeSession preludeSession "main.lam"
               "import A\nimport B\nA.a + B.b") []
      fmap snd (loaded >>= finalText) `shouldBe` Right (Just "23 : Int")
      reverse readLog `shouldBe` ["A.lam", "C.lam", "B.lam"]
    -- ANCHOR_END: once

    -- ANCHOR: moduleErrors
    -- 「モジュールのファイルのまちがいを、場所と一緒に教える」
    it "tells what is wrong with a module file, and where" $ do
      let with m = runModules [("M.lam", m)]
      with "x = 1" "import M\n1" `shouldBe` Left
        ("M.lam:1:1: error: a module starts with the line  "
         <> "module M exposing (...)")
      with "module N exposing (x)\nx = 1" "import M\n1" `shouldBe`
        Left "M.lam:1:1: error: the file M.lam must be the module M, not N"
      with "module M exposing (y)\nx = 1" "import M\n1" `shouldBe`
        Left ("M.lam:1:1: error: the module M exposes y, "
              <> "but does not define it")
      with "module M exposing (x)\nx = 1\nx" "import M\n1" `shouldBe`
        Left ("M.lam: error: a module cannot end with an expression"
              <> " (only a program can)")
      with "module M exposing (x)\nx = 1 // 0" "import M\n1" `shouldBe`
        Left "M.lam:2:7: in the definition of x: error: divided by zero"
    -- ANCHOR_END: moduleErrors

    -- ANCHOR: clashTests
    -- 「2つのモジュールの同じ名前・同じ型は、エラーにする」
    it "stops when two modules give the same name or the same type" $ do
      let box m n = T.unlines
            [ "module " <> m <> " exposing (Box (..), size)"
            , "data Box = Box Int", "size = " <> n ]
          go = runModules [("A.lam", box "A" "1"), ("B.lam", box "B" "2")]
      go "import A (size)\nimport B (size)\n1"
        `shouldBe` Left
          "main.lam:2:1: error: the name size comes from both A and B"
      go "import A\nimport B\n1" `shouldBe` Left
        "main.lam:2:1: error: the type Box comes from both A and B"
      -- Only the names in front differ: A.size and B.size are fine.
      -- 前の名前だけがちがうなら、A.size と B.size は大丈夫
      go "import A as X\nX.size" `shouldBe` Right "1 : Int"
      runModules [] "data Maybe a = Nope\n1" `shouldBe` Left
        ("in the data type Maybe: type error: "
         <> "the type Maybe belongs to the module Maybe")
      runModules [] "class Display a where\n  display : a -> String\n1"
        `shouldBe` Left ("in the class Display: type error: "
                         <> "the class Display belongs to the module Prelude")
    -- ANCHOR_END: clashTests

    -- ANCHOR: ringTest
    -- 「import の輪は、ぐるぐる回らずにエラーにする」
    it "finds imports that go around in a ring" $
      runModules
        [ ("A.lam", "module A exposing (a)\nimport B\na = 1")
        , ("B.lam", "module B exposing (b)\nimport A\nb = 2") ]
        "import A\n1"
        `shouldBe` Left ("B.lam:2:1: error: the imports go around in a "
                         <> "ring: main.lam -> A.lam -> B.lam -> A.lam")
    -- ANCHOR_END: ringTest

    -- ANCHOR: replImport
    -- 「REPL の import と :browse」
    it "imports and browses in the REPL" $ do
      out <- replImporting [("Shapes.lam", shapes)]
        [ "import Shapes as S (area)", "area (S.square 3)", ":browse Shapes"
        , "import Nope" ]
      out `shouldBe`
        [ "", "9 : Int"
        , T.intercalate "\n"
            [ "Shapes.Circle : Int -> Shape"
            , "Shapes.Rect : Int -> Int -> Shape"
            , "Shapes.area : Shape -> Int"
            , "Shapes.describe : Describe a => a -> String"
            , "Shapes.square : Int -> Shape"
            , "(instances: Describe Shape, Display Shape)" ]
        , "<repl>:1:1: error: no module Nope (looked for Nope.lam)" ]
    -- ANCHOR_END: replImport

    -- ANCHOR: exposedProp
    -- 「exposing に書いた名前だけが、ほかのファイルから見える」
    it "shows exactly the names in the exposing list" $
      property $ forAll (sublistOf ["one", "two", "three"]) $ \shown ->
        let header =
              "module Nums exposing (" <> T.intercalate ", " shown <> ")"
            nums = T.unlines [ header, "one = 1", "two = 2", "three = 3" ]
            seen n = runModules [("Nums.lam", nums)]
                       ("import Nums\nNums." <> n)
                       /= Left ("type error: unknown name: Nums." <> n)
        in not (null shown) ==>
             [ n | n <- ["one", "two", "three"], seen n ] === shown
    -- ANCHOR_END: exposedProp

  -- 「演算子を作る」
  describe "operators" $ do
    -- ANCHOR: operatorGolden
    mapM_ (golden . ("operators/" <>))
      [ "basket", "strength", "price/today", "price/short"
      , "broken/unknown", "broken/mixed", "broken/typo", "broken/arrow"
      , "broken/peek" ]
    -- ANCHOR_END: operatorGolden

    -- ANCHOR: infixTests
    -- 「infixl の行を、宣言に読む」
    it "reads an infixl line into a declaration" $
      runLamParser ops (infixDecl <* eof) "m.lam" "infixl 7 addPrice as <+>"
        `shouldBe` Right (DefInfix (Loc "m.lam" 1 1) "<+>"
                            (Fixity LeftA 7 "addPrice" False))

    -- 「1回目の読みこみは、infix の行だけを、どこにあっても集める」
    it "finds the infix lines first, wherever they are" $ do
      fmap (map (\(_, o, f) -> (o, fixityTarget f)))
        (fixitiesOf "m.lam" (T.unlines
           [ "x = 1 <+> 2", "-- infixl 1 no as <->", "infixr 5 add as <+>"
           , "y = \"infixl 1 no as <->\"" ]))
        `shouldBe` Right [("<+>", "add")]
      runModules [] (T.unlines
        [ "plus a b = a + b", "x = 1 <+> 2", "infixl 6 plus as <+>", "x" ])
        `shouldBe` Right "3 : Int"

    -- 「自分の演算子は、関数の呼び出しに開く」
    it "opens an operator of your own into a call of its function" $
      fmap render (runLamParser
        (Map.insert "<+>" (Fixity LeftA 7 "addPrice" False) ops)
        (expr <* eof) "" "a <+> b <+> c")
        `shouldBe` Right "addPrice (addPrice a b) c"
    -- ANCHOR_END: infixTests

    -- ANCHOR: levelTests
    -- 「強さと寄る向き」
    it "reads the levels and the leaning of the operators" $ do
      let with decl e = runModules []
            (T.unlines [ "minus a b = a - b", decl, e ])
      with "infixl 6 minus as <->" "10 <-> 3 <-> 2" `shouldBe` Right "5 : Int"
      with "infixr 6 minus as <->" "10 <-> 3 <-> 2" `shouldBe` Right "9 : Int"
      -- tighter than *, looser than +
      -- * より強い、+ より弱い
      with "infixl 9 minus as <->" "2 * 10 <-> 3" `shouldBe` Right "14 : Int"
      with "infixl 6 minus as <->" "10 <-> 3 + 2" `shouldBe` Right "5 : Int"

    -- 「続けて書けない鎖と、混ぜられない鎖」
    it "says so for a chain of == and for a mixed chain" $ do
      runLines ["1 == 1 == 1"]
        `shouldSatisfy` all ("== cannot be chained" `T.isInfixOf`)
      runLines ["(\\x -> x) |> (\\x -> x) <| 1"]
        `shouldSatisfy` all ("cannot mix |> and <|" `T.isInfixOf`)
      runLines ["x = 1 <+> 2"]
        `shouldSatisfy` all ("unknown operator <+>" `T.isInfixOf`)
    -- ANCHOR_END: levelTests

    -- ANCHOR: chainProp
    -- 「1つの段の鎖は、左に寄れば foldl、右に寄れば foldr」
    it "folds a chain from the left or from the right" $
      property $ forAll (choose (1, 6)) $ \n ->
        forAll (vectorOf n (choose (0, 99 :: Integer))) $ \xs ->
        forAll (elements [("infixl", True), ("infixr", False)]) $
          \(word, left) ->
            let source = T.unlines
                  [ "minus a b = a - b", word <> " 5 minus as <->"
                  , T.intercalate " <-> " (map (T.pack . show) xs) ]
                expected
                  | left      = foldl1 (-) xs
                  | otherwise = foldr1 (-) xs
            in runModules [] source
                 === Right (T.pack (show expected) <> " : Int")
    -- ANCHOR_END: chainProp

    -- ANCHOR: preludeTests
    -- 「組み込みの演算子は、どれもプレリュードで1度だけ宣言する」
    it "declares every builtin operator once, in the Prelude" $ do
      let declared = Map.elems ops
      sortOn id [ fixityTarget f | f <- declared, fixityBuiltin f ]
        `shouldBe` Map.keys builtinOperators
      all (\f -> fixityLevel f >= 0 && fixityLevel f <= 9) declared
        `shouldBe` True

    -- 「組み込みの演算子の関数も、名前で呼べる」
    it "calls the functions of the builtin operators by name" $
      runLines [ "add 2 3", "cons 1 [2]", "pipe 3 (add 1)"
               , ":type lessThan", "and true false" ]
        `shouldBe` [ "5 : Int", "[1, 2] : [Int]", "4 : Int"
                   , "Comparable a => a -> a -> Bool", "false : Bool" ]
    -- ANCHOR_END: preludeTests

    -- ANCHOR: reservedTests
    -- 「言語の記号と、0〜9 でない強さ」
    it "refuses the symbols of the language and a level over 9" $
      runLines [ "infixl 5 f as =", "infixl 5 f as <-", "infixl 5 f as |"
               , "infixl 10 f as <+>" ]
        `shouldSatisfy` \outs ->
          and (zipWith T.isInfixOf
                [ "= belongs to the language", "<- belongs to the language"
                , "| belongs to the language", "the level of an operator is"
                ] outs)
    -- ANCHOR_END: reservedTests

    -- ANCHOR: operatorModules
    -- 「演算子を見せて、import する」
    it "shows operators from a module, and imports them" $ do
      let money = T.unlines
            [ "module Money exposing (plus, (<+>))"
            , "plus a b = a + b", "infixl 6 plus as <+>" ]
          other = T.unlines
            [ "module Other exposing (also, (<+>))"
            , "also a b = a * b", "infixl 6 also as <+>" ]
          run = runModules [("Money.lam", money), ("Other.lam", other)]
      run "import Money ((<+>))\n1 <+> 2" `shouldBe` Right "3 : Int"
      -- as M: <+> calls M.plus
      -- as M なら、<+> は M.plus を呼ぶ
      run "import Money as M ((<+>))\n(\\x -> x <+> 1)"
        `shouldBe` Right "<function of x> : Int -> Int"
      -- Without the list, the operator does not come.
      -- 一覧がなければ、演算子は来ない
      run "import Money\n1 <+> 2"
        `shouldSatisfy` either ("unknown operator <+>" `T.isInfixOf`)
                               (const False)
      run "import Money ((<->))\n1"
        `shouldBe` Left "main.lam:1:1: error: the module Money does not \
                        \expose (<->)"
      run "import Money ((<+>))\nimport Other ((<+>))\n1"
        `shouldBe` Left "main.lam:2:1: error: the operator <+> comes from \
                        \both Money and Other"
    -- ANCHOR_END: operatorModules

    -- ANCHOR: replInfix
    -- 「REPL で演算子を作り、:browse で見る」
    it "makes an operator in the REPL, and shows them in :browse" $ do
      runLines [ "infixr 5 max as <^>", "1 <^> 5 <^> 3" ]
        `shouldBe` ["infixr 5 max as <^>", "5 : Int"]
      outs <- replImporting [("Money.lam", T.unlines
                [ "module Money exposing (plus, (<+>))"
                , "plus a b = a + b", "infixl 6 plus as <+>" ])]
                [ "import Money ((<+>))", ":browse Money" ]
      outs `shouldBe`
        [ "", "Money.plus : Number a => a -> a -> a\ninfixl 6 plus as <+>" ]
    -- ANCHOR_END: replInfix


  -- 「IO と do」
  describe "IO and do" $ do
    -- ANCHOR: ioTypes
    -- 「IO の値の型。IO は比べられない」
    it "gives IO values their types" $
      runLines
        [ ":type putStrLn", ":type getLine", ":type readFile"
        , ":type IO.putStrLn"
        , "putStrLn \"hi\" == putStrLn \"hi\"" ]
        `shouldBe`
          [ "String -> IO ()", "IO (Maybe String)"
          , "String -> IO (Result String String)", "String -> IO ()"
          , "type error: IO () is not in the class Comparable" ]
    -- ANCHOR_END: ioTypes

    -- ANCHOR: monadTypes
    -- 「pure と ap は Applicative、bind は Monad のメソッド」
    it "makes pure and ap the methods of Applicative, and bind of Monad" $
      runLines
        [ ":type pure", ":type ap", ":type bind"
        , ":type forEach", ":type mapM" ]
        `shouldBe`
          [ "Applicative f => a -> f a"
          , "Applicative f => f (a -> b) -> f a -> f b"
          , "Monad f => f a -> (a -> f b) -> f b"
          , "Monad f => [a] -> (a -> f b) -> f ()"
          , "Monad f => (a -> f b) -> [a] -> f [b]" ]
    -- ANCHOR_END: monadTypes

    -- ANCHOR: doSugar
    -- 「do は、隠した名前の bind と関数に開く」
    it "opens do into the hidden bind and functions" $
      map (fmap render . parseExpr)
        [ "do x <- getLine; putStrLn x"
        , "do putStr \"a\"; let b = \"b\"; putStrLn b"
        , "do\n  (a, b) <- pure (1, 2)\n  pure (a + b)" ]
        `shouldBe` map Right
          [ "$do.bind getLine (\\x -> putStrLn x)"
          , "$do.bind (putStr \"a\") (\\$_ -> let b = \"b\" in putStrLn b)"
          , "$do.bind (pure (1, 2)) (\\$arg -> case $arg of \
            \| (a, b) -> pure (a + b))" ]
    -- ANCHOR_END: doSugar

    -- ANCHOR: doErrors
    -- 「do の最後の行は式。; で1行に並べてもよい」
    it "needs an expression at the end of do" $
      runLines
        [ "do x <- pure 1", ":type do x <- pure 1; pure (x + 1)" ]
        `shouldBe`
          [ "<repl>:1:15:\n  |\n1 | do x <- pure 1\n  |               ^\n\
            \the last line of do must be an expression\n"
          , "(Monad f, Number a) => f a\n  -- usually : Monad f => f Int" ]
    -- ANCHOR_END: doErrors

    -- ANCHOR: doMonads
    -- 「do は、Maybe でも Result でもリストでも使える」
    it "uses do for Maybe, Result and lists too" $
      runLines
        [ "do x <- Just 1; y <- Just 2; pure (x + y)"
        , "do x <- Just 1; y <- Nothing; pure (x + y)"
        , "do x <- Ok 1; y <- Err \"no y\"; pure (x + y)"
        , "do x <- [1, 2]; y <- [\"a\", \"b\"]; pure (x, y)"
        , "mapM (\\x -> if x > 0 then Just x else Nothing) [3, 1, 2]" ]
        `shouldBe`
          [ "Just 3 : Maybe Int", "Nothing : Maybe Int"
          , "Err \"no y\" : Result String Int"
          , "[(1, \"a\"), (1, \"b\"), (2, \"a\"), (2, \"b\")] : [(Int, String)]"
          , "Just [3, 1, 2] : Maybe [Int]" ]
    -- ANCHOR_END: doMonads

    -- ANCHOR: applicativeOps
    -- 「<$> と <*> で、箱の中の値を1つの関数に渡す」
    it "gives the values in boxes to one function with <$> and <*>" $
      runLines
        [ ":type (<$>)", ":type (<*>)"
        , "(+) <$> Just 1 <*> Just 2"
        , "(+) <$> Just 1 <*> Nothing"
        , "(+) <$> Ok 1 <*> Err \"no b\""
        , "(+) <$> [1, 2] <*> [10, 20]"
        , "(+) <$> Just 1 <*> Just 2 == Just 3" ]
        `shouldBe`
          [ "Functor f => (a -> b) -> f a -> f b"
          , "Applicative f => f (a -> b) -> f a -> f b"
          , "Just 3 : Maybe Int", "Nothing : Maybe Int"
          , "Err \"no b\" : Result String Int"
          , "[11, 21, 12, 22] : [Int]"
          , "true : Bool" ]
    -- ANCHOR_END: applicativeOps

    -- ANCHOR: adoSugar
    -- 「ado は、隠した名前の map と ap に開く」
    it "opens ado into the hidden map and ap" $
      map (fmap render . parseExpr)
        [ "ado x <- Just 1; y <- Just 2; pure (x + y)"
        , "ado putStr \"a\"; s <- getLine; pure s"
        , "ado pure 3" ]
        `shouldBe` map Right
          [ "$do.ap ($do.map (\\x -> \\y -> x + y) (Just 1)) (Just 2)"
          , "$do.ap ($do.map (\\$_ -> \\s -> s) (putStr \"a\")) getLine"
          , "$do.pure 3" ]
    -- ANCHOR_END: adoSugar

    -- ANCHOR: adoErrors
    -- 「ado の行は、上の行の名前を使えない。最後は pure」
    it "says why an ado cannot be opened" $
      runLines
        [ "ado x <- Just 1; y <- Just x; pure y"
        , "ado x <- Just 1; x" ]
        `shouldBe`
          [ "<repl>:1:37:\n  |\n1 | ado x <- Just 1; y <- Just x; pure y\n\
            \  |                                     ^\n\
            \in an ado, a line cannot use x from a line above (use do)\n"
          , "<repl>:1:19:\n  |\n1 | ado x <- Just 1; x\n\
            \  |                   ^\n\
            \the last line of an ado must be pure, then an expression\n" ]
    -- ANCHOR_END: adoErrors

    -- ANCHOR: adoTypes
    -- 「ado は Applicative だけ、do は Monad を求める」
    it "needs only an Applicative for an ado" $
      runLinesIn emptyWorld { worldInput = ["Lam", "3"] }
        [ "both a b = ado x <- a; y <- b; pure (x, y)"
        , "bothDo a b = do x <- a; y <- b; pure (x, y)"
        , "both (Just 1) (Just \"one\")"
        , "both getLine getLine" ]
        `shouldBe`
          ( [ "both : Applicative f => f a -> f b -> f (a, b)"
            , "bothDo : Monad f => f a -> f b -> f (a, b)"
            , "Just (1, \"one\") : Maybe (Int, String)"
            , "(Just \"Lam\", Just \"3\") : (Maybe String, Maybe String)" ]
          , emptyWorld )
    -- ANCHOR_END: adoTypes

    -- ANCHOR: makingIsNotDoing
    -- 「作るだけでは、何も起きない。やるたびに起きる」
    it "does nothing when an action is made, and does it each time" $
      runLinesIn emptyWorld
        [ "hi = putStrLn \"hi\""
        , "[hi, hi, hi]"
        , "do hi; hi" ]
        `shouldBe`
          ( [ "hi : IO ()"
            , "[<IO action>, <IO action>, <IO action>] : [IO ()]"
            , "" ]
          , emptyWorld { worldOutput = ["hi\n", "hi\n"] } )
    -- ANCHOR_END: makingIsNotDoing

    -- ANCHOR: replIO
    -- 「REPL は、IO の式をその場でやる。pure 1 も IO として」
    it "does an IO in the REPL, and takes pure 1 as an IO" $
      runLinesIn emptyWorld
        [ "pure 1", "pure (1, \"one\")", "map (\\x -> x + 1) (pure 3)"
        , "forEach [\"a\", \"b\"] (\\s -> putStrLn (toUpper s))" ]
        `shouldBe`
          ( [ "1 : Int", "(1, \"one\") : (Int, String)", "4 : Int", "" ]
          , emptyWorld { worldOutput = ["B\n", "A\n"] } )
    -- ANCHOR_END: replIO

    -- ANCHOR: hiddenBind
    -- 「自分で bind を定義しても、do は Monad の bind を呼ぶ」
    it "calls the bind of Monad, even with a bind of your own" $
      runLinesIn emptyWorld
        [ "hi = putStrLn \"hi\"", "bind m f = 42", "do hi; hi" ]
        `shouldBe`
          ( [ "hi : IO ()", "bind : Number c => a -> b -> c", "" ]
          , emptyWorld { worldOutput = ["hi\n", "hi\n"] } )
    -- ANCHOR_END: hiddenBind

    -- ANCHOR: fakeFiles
    -- 「偽物の世界のファイルを、読んで書く」
    it "reads and writes the files of a fake world" $ do
      let world = emptyWorld
            { worldFiles = textFiles [("memo.txt", "λ memo\n")] }
          (outs, world') = runLinesIn world
            [ "readFile \"memo.txt\""
            , "readFile \"nothing.txt\""
            , "writeFile \"new.txt\" \"hello\"" ]
      outs `shouldBe`
        [ "Ok \"λ memo\\n\" : Result String String"
        , "Err \"nothing.txt: no such file\" : Result String String"
        , "Ok () : Result String ()" ]
      worldFiles world' `shouldBe`
        textFiles [("memo.txt", "λ memo\n"), ("new.txt", "hello")]
    -- ANCHOR_END: fakeFiles

    -- ANCHOR: echo
    -- 「読んだ行を、そのまま全部書き出す（でたらめな行で）」
    it "echoes every line it reads" $
      forAll (listOf (T.filter (/= '\n') <$> genText)) $ \ls ->
        let echo = "echo = do line <- getLine; case line of \
                   \| Nothing -> pure () | Just s -> do putStrLn s; echo"
            (_, world') = runLinesIn emptyWorld { worldInput = ls }
                            [echo, "echo"]
        in outputOf world' === T.concat [ l <> "\n" | l <- ls ]
    -- ANCHOR_END: echo

    -- ANCHOR: shopFake
    -- 「shop.lam は、偽物の世界に receipt.txt を書く。注文が
    --   壊れていれば、そう言う」
    it "writes receipt.txt into a fake world, or says what is wrong" $ do
      source  <- Utf8.readFile "examples/io/shop.lam"
      receipt <- Utf8.readFile "examples/modules/receipt/Receipt.lam"
      orders  <- Utf8.readFile "examples/io/orders.json"
      let modules = mapFiles
            (Map.fromList [("lib/Receipt.lam", receipt)]) ["lib"]
          run json = runFile modules emptyWorld
            { worldFiles = textFiles [("examples/io/orders.json", json)] }
            "shop.lam" source
          (result, world) = run orders
      result `shouldBe` Right ()
      fmap (take 2 . T.lines . decodeUtf8)
          (Map.lookup "receipt.txt" (worldFiles world))
        `shouldBe` Just [ "             RECEIPT              "
                        , "sticker     4 x    300 =     1,200" ]
      map (outputOf . snd . run)
        ["{\"sticker\": 4}", "[{\"item\": \"mug\"}]", "[1,"]
        `shouldBe`
          [ "no receipt: the orders must be a list\n"
          , "no receipt: each order needs an item and a count\n"
          , "no receipt: json list value: not enough input\n" ]
    -- ANCHOR_END: shopFake

    -- ANCHOR: checkMain
    -- 「main は IO () でなければならない」
    it "says when main is not IO ()" $
      map (\src -> fst (runFile (mapFiles Map.empty []) emptyWorld "m" src))
        ["main = 1", "main = putStrLn", "main = pure 3"]
        `shouldBe` map (Left . ("in main: type error: " <>))
          [ "the types do not fit: a number where IO () is needed"
          , "main must be IO (), [String] -> IO (), [String] -> \
            \Map String String -> IO (), a record -> IO (), or a record of \
            \those (or IO (Result e ()) in place of IO ()), but it is \
            \String -> IO ()"
          , "the types do not fit: a number where () is needed" ]
    -- ANCHOR_END: checkMain

    -- ANCHOR: ioGolden
    mapM_ (golden . ("io/" <>)) ["hello", "shop", "ado"]
    -- ANCHOR_END: ioGolden

  -- 「失敗を早めに返す：? と後始末」
  describe "errors" $ do
    -- ANCHOR: trySugar
    -- 「? の行は、branch で開く case に。失敗なら、すぐに pure で返す」
    it "opens a line with ? into a case on branch" $
      map (fmap render . parseExpr)
        [ "do t <- readFile p ?; putStrLn t"
        , "do writeFile p t ?; pure (Ok ())" ]
        `shouldBe` map Right
          [ "$do.bind (readFile p) (\\$try -> case $try.branch $try of \
            \| Ok t -> putStrLn t | Err $failed -> $do.pure $failed)"
          , "$do.bind (writeFile p t) (\\$try -> case $try.branch $try of \
            \| Ok _ -> pure (Ok ()) | Err $failed -> $do.pure $failed)" ]
    -- ANCHOR_END: trySugar
    -- ANCHOR: deferSugar
    -- 「defer の行は、残りの行を $do.finally で包む」
    it "opens defer into the hidden finally around the rest" $
      fmap render (parseExpr "do defer putStrLn \"bye\"; putStrLn \"hi\"")
        `shouldBe` Right
          "$do.finally ($do.bind ($do.pure ()) (\\$_ -> putStrLn \"hi\")) \
          \(putStrLn \"bye\")"
    -- ANCHOR_END: deferSugar
    -- ANCHOR: tryErrors
    -- 「最後の行に ? や defer は書けない。?: と /? は、これまでどおり」
    it "says where ? and defer cannot go, and keeps ?: and /?" $
      runLines
        [ "do putStrLn \"a\" ?"
        , "do putStrLn \"a\"; defer putStrLn \"b\""
        , "do x <- pure (Nothing ?: 1); pure (x + 1)"
        , "do x <- pure (7 /? 0); pure x"
        , "deferred = 3" ]
        `shouldBe`
          [ "<repl>:1:18:\n  |\n1 | do putStrLn \"a\" ?\n  |                  ^\n\
            \the last line of do is the answer itself: write it without ?\n"
          , "<repl>:1:36:\n  |\n1 | do putStrLn \"a\"; defer putStrLn \"b\"\n\
            \  |                                    ^\n\
            \the last line of do must be an expression\n"
          , "2 : Int"
          , "Nothing : Maybe Float"
          , "deferred : Number a => a\n  -- usually : Int" ]
    -- ANCHOR_END: tryErrors
    -- ANCHOR: tryTypes
    -- 「? を使う do の型。Maybe と Result は混ぜられない」
    it "gives the do with ? the type of the container" $
      runLines
        [ ":type \\p -> do t <- readFile p ?; pure (Ok (length t))"
        , ":type do n <- getLine ?; pure (Just (length n))"
        , ":type branch"
        , ":type Result.mapError"
        , ":type do n <- getLine ?; t <- readFile n ?; pure (Ok t)" ]
        `shouldBe`
          [ "String -> IO (Result String Int)"
          , "IO (Maybe Int)"
          , "Fallible f => f a -> Result (f b) a"
          , "(a -> b) -> Result a c -> Result b c"
          , "type error: the types do not fit: \
            \Result String String where Maybe String is needed" ]
    -- ANCHOR_END: tryTypes
    -- ANCHOR: tryStops
    -- 「? は、最初の失敗で止まる。そのあとの行は、やらない」
    it "stops at the first failure, and does not do the lines after it" $
      runLinesIn emptyWorld
            { worldFiles = textFiles [("a.txt", "apple")] }
        [ "do a <- readFile \"a.txt\" ?; putStrLn a; \
          \b <- readFile \"b.txt\" ?; putStrLn b; pure (Ok ())"
        , "do x <- Just (Just 1) ?; y <- Just Nothing ?; pure (Just (x + y))" ]
        `shouldBe`
          ( [ "Err \"b.txt: no such file\" : Result String ()"
            , "Just Nothing : Maybe (Maybe Int)" ]
          , emptyWorld { worldFiles = textFiles [("a.txt", "apple")]
                       , worldOutput = ["apple\n"] } )
    -- ANCHOR_END: tryStops
    -- ANCHOR: tryProperty
    -- 「? を並べた do は、Haskell の sequence と同じ答え。画面に出るのは、
    --   最初の Err の手前まで」
    it "gives the answer of sequence, and does the lines up to the first Err" $
      forAll (listOf genResult) $ \rs ->
        let line i r = named "x" i <> " <- pure (" <> lamResult r <> ") ?; \
                       \putStrLn \"" <> num (toInteger i) <> "\""
            xs = [ named "x" i | i <- [1 .. length rs] ]
            source = "do " <> T.concat [ line i r <> "; "
                                       | (i, r) <- zip [1 ..] rs ]
                       <> "pure (Ok [" <> T.intercalate ", " xs <> "])"
            (outs, world) = runLinesIn emptyWorld [source]
            done = length (takeWhile isRight rs)
        in (map valuePart outs, outputOf world)
             === ( [lamResultList (sequence rs)]
                 , T.concat [ num i <> "\n" | i <- [1 .. toInteger done] ] )
    -- ANCHOR_END: tryProperty
    -- ANCHOR: stopTests
    -- 「error と todo は、どんな型にもなれて、止まる」
    it "stops with error and todo, which can be of any type" $
      runLines
        [ ":type error"
        , "if 1 > 2 then error \"never\" else 3"
        , "1 + error \"boom\""
        , "length [todo \"a\", \"b\"]"
        , "f x = todo \"f\""
        , "f 3" ]
        `shouldBe`
          [ "String -> a", "3 : Int", "error: boom"
          , "error: not written yet: a", "f : a -> b"
          , "error: not written yet: f" ]
    -- ANCHOR_END: stopTests
    -- ANCHOR: finallyTests
    -- 「後始末は、止まっても、? で返っても、やる。defer は後ろから」
    it "cleans up after a stop and after ?, the last defer first" $
      runLinesIn emptyWorld
        [ "IO.finally (putStrLn \"body\") (putStrLn \"cleanup\")"
        , "IO.finally (do pure (); pure (1 // 0)) (putStrLn \"cleanup\")"
        , "do defer putStrLn \"1\"; defer putStrLn \"2\"; putStrLn \"body\""
        , "do defer putStrLn \"bye\"; t <- readFile \"none\" ?; pure (Ok t)"
        , "do defer putStrLn \"bye\"; x <- pure (1 // 0); pure x" ]
        `shouldBe`
          ( [ "", "error: divided by zero", ""
            , "Err \"none: no such file\" : Result String String"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = reverse
              [ "body\n", "cleanup\n", "cleanup\n", "body\n", "2\n", "1\n"
              , "bye\n", "bye\n" ] } )
    -- 「後始末も止まったら、そちらを伝える（Haskell と同じ）」
    it "tells the stop of the cleanup, when both stop" $
      runLines [ "IO.finally (do pure (); error \"body\") (error \"cleanup\")" ]
        `shouldBe` [ "error: cleanup" ]
    -- ANCHOR_END: finallyTests
    -- ANCHOR: deferMany
    -- 「defer を3つ。後のものから動き、後始末が止まっても外側は動く」
    it "runs many defers, the last first, even when a cleanup stops" $
      runLinesIn emptyWorld
        [ "bye n = putStrLn f\"bye {n}\""
        , "do defer bye 1; defer bye 2; defer bye 3; putStrLn \"body\""
        , "do defer bye 1; defer (do pure (); pure (1 // 0)); \
          \putStrLn \"body\""
        , "do defer bye 1; defer (pure (1 // 0)); putStrLn \"body\"" ]
        `shouldBe`
          ( [ "bye : Display a => a -> IO ()", "", "error: divided by zero"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = reverse
              [ "body\n", "bye 3\n", "bye 2\n", "bye 1\n"
              , "body\n", "bye 1\n", "bye 1\n" ] } )
    -- ANCHOR_END: deferMany
    -- ANCHOR: bracketTry
    -- 「bracket は返すのを忘れない。try は、やる途中で止まったものだけを
    --   つかまえる」
    it "gives back with bracket, and catches stops while doing with try" $
      runLinesIn emptyWorld
        [ "IO.bracket (pure 3) (\\r -> putStrLn f\"back {r}\") \
          \(\\r -> pure (r * 2))"
        , "IO.bracket (pure 3) (\\r -> putStrLn f\"back {r}\") \
          \(\\r -> pure (r // 0))"
        , "IO.try (do pure (); pure (1 // 0))"
        , "IO.try (pure 5)"
        , "IO.try (pure (1 // 0))" ]
        `shouldBe`
          ( [ "6 : Int", "error: divided by zero"
            , "Err \"divided by zero\" : Result String Int"
            , "Ok 5 : Result String Int"
            , "error: divided by zero" ]
          , emptyWorld { worldOutput = ["back 3\n", "back 3\n"] } )
    -- ANCHOR_END: bracketTry
    -- ANCHOR: mainResult
    -- 「main が Err を返したら、その文で止まる」
    it "stops with the words of an Err from main" $
      map (\src -> fst (runFile (mapFiles Map.empty []) emptyWorld "m" src))
        [ "main : IO (Result String ())\nmain = pure (Err \"no orders\")"
        , "main = pure (Err 404)"
        , "main = pure (Ok ())"
        , "main = pure (Ok 3)" ]
        `shouldBe`
          [ Left "error: no orders", Left "error: 404", Right ()
          , Left "in main: type error: \
                 \the types do not fit: Result a b where () is needed" ]
    -- ANCHOR_END: mainResult
    -- ANCHOR: shopErrors
    -- 「注文が壊れていても、ファイルがなくても、お店は閉まる」
    it "closes the shop after any failure" $ do
      source  <- Utf8.readFile "examples/errors/shop.lam"
      receipt <- Utf8.readFile "examples/modules/receipt/Receipt.lam"
      let modules = mapFiles
            (Map.fromList [("lib/Receipt.lam", receipt)]) ["lib"]
          run files = runFile modules emptyWorld
            { worldFiles = textFiles files } "shop.lam" source
          orders json = [("examples/errors/orders.json", json)]
          tell (result, world) = (result, outputOf world)
      map (tell . run)
        [orders "{\"sticker\": 4}", orders "[1,", []]
        `shouldBe`
          [ ( Left "error: the orders must be a list"
            , "the shop opens\nthe shop closes\n" )
          , ( Left "error: json list value: not enough input"
            , "the shop opens\nthe shop closes\n" )
          , ( Left "error: examples/errors/orders.json: no such file"
            , "the shop opens\nthe shop closes\n" ) ]
    -- ANCHOR_END: shopErrors
    -- ANCHOR: errorsGolden
    mapM_ (golden . ("errors/" <>)) ["shop", "register", "todo"]
    -- ANCHOR_END: errorsGolden

  describe "script" $ do
    -- ANCHOR: argsTests
    -- 「引数と環境変数を、3つのやり方で受け取る」
    it "takes the arguments and the environment, in three ways" $ do
      let world = emptyWorld
            { worldArgs = ["Lam", "Haskell"]
            , worldEnv  = Map.fromList [("LAM_USER", "Lisp")] }
          run src = outputOf (snd (script world src))
      map run
        [ "main = do\n  args <- getArgs\n  putStrLn f\"{args}\""
        , "main args = putStrLn f\"{length args} names\""
        , "main args env = putStrLn (Map.lookup \"LAM_USER\" env ?: \"?\")"
        , "main = do\n  user <- lookupEnv \"NOPE\"\n  putStrLn f\"{user}\"" ]
        `shouldBe`
          [ "[\"Lam\", \"Haskell\"]\n", "2 names\n", "Lisp\n", "Nothing\n" ]
    -- ANCHOR_END: argsTests
    -- ANCHOR: mainShapes
    -- 「引数のある main も、IO (Result e ()) を返せる。形に合わない main は、
    --   許される形を教える」
    it "lets main with arguments give a Result, and tells the shapes" $
      map (fst . script emptyWorld { worldArgs = ["x", "y"] })
        [ "main : [String] -> IO (Result String ())\n\
          \main args = pure (Err (join \",\" args))"
        , "main args env = pure (Ok ())"
        , "main args = pure 3" ]
        `shouldBe`
          [ Errored "error: x,y"
          , Finished ""
          , Errored "in main: type error: main must be IO (), [String] -> \
                    \IO (), [String] -> Map String String -> IO (), a \
                    \record -> IO (), or a record of those (or \
                    \IO (Result e ()) in place of IO ()), but it is \
                    \a -> f b" ]
    -- ANCHOR_END: mainShapes
    -- ANCHOR: exitTests
    -- 「exit で止まり、残りはやらない。標準出力と標準エラーは別々」
    it "stops at exit, and does not do the rest" $ do
      let (ending, world) = script emptyWorld (T.unlines
            [ "main = do"
            , "  putStrLn \"before\""
            , "  ePutStrLn \"oops\""
            , "  exit 3"
            , "  putStrLn \"after\"" ])
      ending `shouldBe` ExitedWith 3
      outputOf world `shouldBe` "before\n"
      errorsOf world `shouldBe` "oops\n"
    -- ANCHOR_END: exitTests
    -- ANCHOR: exitCleanup
    -- 「exit でも後始末はやる。IO.try は exit をつかまえない。後始末も
    --   exit したら、そちらが勝つ」
    it "does the cleanups on exit, and try does not catch it" $
      map (\src -> let (e, w) = script emptyWorld src in (e, outputOf w))
        [ "main = do\n  defer putStrLn \"closed\"\n  exit 4"
        , "main = IO.finally (exit 5) (putStrLn \"finally\")"
        , "main = IO.bracket (pure 1) (\\_ -> putStrLn \"back\") \
          \(\\_ -> exit 6)"
        , "main = do\n  r <- IO.try (exit 7)\n  putStrLn \"caught\""
        , "main = IO.finally (exit 8) (exit 9)" ]
        `shouldBe`
          [ (ExitedWith 4, "closed\n"), (ExitedWith 5, "finally\n")
          , (ExitedWith 6, "back\n"), (ExitedWith 7, "")
          , (ExitedWith 9, "") ]
    -- ANCHOR_END: exitCleanup
    -- ANCHOR: placeTests
    -- 「エラーは、いちばん内側の場所を指す。error と todo も。#! の行が
    --   あっても、行の番号はずれない。REPL は場所を見せない」
    it "points at the innermost place, and keeps the lines after #!" $ do
      map (fst . script emptyWorld)
        [ "half x = x // 0\nmain = do\n  putStrLn \"go\"\n\
          \  putStrLn f\"{half 3}\""
        , "#!/usr/bin/env lam\nmain = do\n  putStrLn \"go\"\n\
          \  error \"boom\""
        , "main = do\n  putStrLn \"go\"\n  todo \"the rest\""
        , "main = do\n  putStrLn (1 + 2)" ]
        `shouldBe`
          [ Errored "script.lam:1:12: error: divided by zero"
          , Errored "script.lam:4:3: error: boom"
          , Errored "script.lam:3:3: error: not written yet: the rest"
          , Errored "script.lam:2:3: in the definition of main: \
                    \type error: the types do not fit: a number where \
                    \String is needed" ]
      runLines ["10 // 0", "error \"boom\""]
        `shouldBe` ["error: divided by zero", "error: boom"]
    -- ANCHOR_END: placeTests
    -- ANCHOR: wordTests
    -- 「行と単語に切る。文字を見分ける。文字列を数に」
    it "cuts lines and words, tells letters, and reads numbers" $
      runLines
        [ "lines \"a\\nb c\\n\\nd\""
        , "words \"  a  b\\tc\\n\""
        , "map String.isLetter [\"a\", \"é\", \"λ\", \"1\", \",\", \"\"]"
        , "map String.toInt [\"42\", \"-7\", \"4x\", \"\", \"-\"]" ]
        `shouldBe`
          [ "[\"a\", \"b c\", \"\", \"d\"] : [String]"
          , "[\"a\", \"b\", \"c\"] : [String]"
          , "[true, true, true, false, false, false] : [Bool]"
          , "[Just 42, Just (-7), Nothing, Nothing, Nothing] : [Maybe Int]" ]
    -- ANCHOR_END: wordTests
    -- ANCHOR: wordfreqProp
    -- 「wordfreq.lam は、Haskell で数えたトップ10と同じものを見せる」
    it "shows the same top 10 as Haskell does" $
      forAll genWords $ \ls -> ioProperty $ do
        source <- Utf8.readFile "examples/script/wordfreq.lam"
        let world = emptyWorld { worldInput = ls }
        pure (outputOf (snd (script world source)) === haskellTop10 ls)
    -- ANCHOR_END: wordfreqProp
    -- ANCHOR: scriptGolden
    mapM_ (golden . ("script/" <>))
      [ "greet", "echo", "shop", "shout", "wc", "sum", "closing"
      , "average", "wordfreq" ]
    -- ANCHOR_END: scriptGolden

  describe "files, commands and clocks" $ do
    -- ANCHOR: pathTest
    -- 「Path の道具は、Haskell の filepath と同じ答え」
    it "joins and cuts paths as Haskell's filepath does" $
      property $ forAll (elements ["logs", "a/b", "a.tar.gz", "x/y.log", "."])
        $ \p -> forAll (elements ["a.log", "b", "c.txt"]) $ \q ->
          map valuePart (runLines
            [ "Path.join " <> renderString p <> " " <> renderString q
            , "Path.basename " <> renderString p
            , "Path.dirname " <> renderString p
            , "Path.extension " <> renderString p
            , "Path.withExtension \".bak\" " <> renderString p ])
            === map (renderString . T.pack)
                  [ T.unpack p </> T.unpack q, takeFileName (T.unpack p)
                  , takeDirectory (T.unpack p), takeExtension (T.unpack p)
                  , replaceExtension (T.unpack p) ".bak" ]
    -- ANCHOR_END: pathTest

    -- ANCHOR: folderTest
    -- 「偽物の世界のフォルダー：作る、見る、名前を変える、消す」
    it "makes, lists, renames and removes in the fake world" $ do
      let world = emptyWorld { worldFiles = textFiles
            [("logs/a.log", "a"), ("logs/b.txt", "b")] }
          (out, world') = runLinesIn world
            [ "File.list \"logs\"", "File.list \"./logs/../logs\""
            , "File.glob \"logs/*.log\"", "File.makeDir \"old/2026\""
            , "File.rename \"logs/a.log\" \"old/2026/a.log\""
            , "File.list \"old\"", "File.remove \"logs\""
            , "File.remove \"logs/b.txt\"", "File.remove \"logs\""
            , "File.isDir \"logs\"", "File.exists \"old/2026/a.log\""
            , "File.list \"nope\"" ]
      map valuePart out `shouldBe`
        [ "Ok [\"a.log\", \"b.txt\"]", "Ok [\"a.log\", \"b.txt\"]"
        , "Ok [\"logs/a.log\"]", "Ok ()", "Ok ()", "Ok [\"2026\"]"
        , "Err \"logs: the folder is not empty\"", "Ok ()", "Ok ()"
        , "false", "true"
        , "Err \"nope: no such folder\"" ]
      Map.keys (worldFiles world') `shouldBe` ["old/2026/a.log"]
    -- ANCHOR_END: folderTest

    -- ANCHOR: infoTest
    -- 「File.info はレコード。大きさは UTF-8 のバイト」
    it "gives the size in UTF-8 bytes, and when it was changed" $
      map valuePart (fst (runLinesIn emptyWorld
        { worldFiles = textFiles [("a.txt", "ラムダ\n")]
        , worldTimes = Map.fromList [("a.txt", 0)] }
        [ "File.info \"a.txt\"", "File.info \"b.txt\"" ]))
        `shouldBe`
          [ "Ok { modified = 1970-01-01T00:00:00Z, size = 10 }"
          , "Err \"b.txt: no such file\"" ]
    -- ANCHOR_END: infoTest

    -- ANCHOR: cleanupDryRun
    -- 「--dry-run の片付け：何も消さずに、消すはずのものを知らせる」
    it "removes nothing with --dry-run, and tells what it would" $ do
      source <- Utf8.readFile "examples/files/cleanup.lam"
      logs   <- mapM (\(p, t) -> (\x -> (p, (x, t))) <$> Utf8.readFile (T.unpack p))
                  [ ("examples/files/logs/app-2026-11-01.log", 1793524200000)
                  , ("examples/files/logs/app-2026-11-17.log", 1794881400000) ]
      let world = emptyWorld
            { worldFiles = textFiles [ (p, x) | (p, (x, _)) <- logs ]
            , worldTimes = Map.fromList [ (p, t) | (p, (_, t)) <- logs ]
            , worldArgs  = ["--dry-run", "examples/files/logs"] }
          (ending, done) = script world source
      ending `shouldBe` Finished ""
      Map.keys (worldFiles done) `shouldBe` map fst logs
      outputOf done `shouldBe` T.unlines
        [ "would remove examples/files/logs/app-2026-11-01.log"
            <> " (15 days, 66 bytes)"
        , "1 of 2 files: 7 days or more" ]
    -- ANCHOR_END: cleanupDryRun

    -- ANCHOR: processTest
    -- 「Process：3つの終わり方と、output」
    it "tells the three endings of a command apart" $ do
      let world = emptyWorld { worldCommands = Map.fromList
            [ (["ls"], (0, "a\n", "")), (["ls", "x"], (1, "", "no x\n")) ] }
      map valuePart (fst (runLinesIn world
        [ "Process.run \"ls\" []", "Process.run \"ls\" [\"x\"]"
        , "Process.run \"nope\" []"
        , "Process.output (Failed { code = 1, out = \"\", err = \"no x\\n\" })" ]))
        `shouldBe`
          [ "Succeeded { err = \"\", out = \"a\\n\" }"
          , "Failed { code = 1, err = \"no x\\n\", out = \"\" }"
          , "CannotStart \"nope: no such command\""
          , "Err \"exit 1: no x\"" ]
    -- ANCHOR_END: processTest

    -- ANCHOR: timeTest
    -- 「Time：ISO 8601 で書いて読み戻すと同じ。差は切り捨て」
    it "reads back the ISO 8601 it writes" $
      property $ forAll (chooseInteger (0, 4102444800000)) $ \ms ->
        map valuePart (runLines
          [ "Time.fromIso (Time.toIso (Time.fromMillis "
              <> T.pack (show ms) <> ")) == Ok (Time.fromMillis "
              <> T.pack (show ms) <> ")" ]) === ["true"]
    it "reads a zone, and cuts the days down" $
      map valuePart (runLines
        [ "Time.fromIso \"2026-11-17T12:00:00+09:00\""
        , "Time.fromIso \"2026-11-17\" |> Result.map Time.toIso"
        , "Time.fromIso \"yesterday\""
        , "Time.diffDays (Time.fromMillis 172799999) (Time.fromMillis 0)"
        , "Time.format Time.utc (Time.fromMillis 0)"
        , "f\"{Time.fromMillis 1500} in {Time.utc}\"" ])
        `shouldBe`
          [ "Ok 2026-11-17T03:00:00Z", "Ok \"2026-11-17T00:00:00Z\""
          , "Err \"not an ISO 8601 time: yesterday\"", "1"
          , "\"1970-01-01 00:00:00 +00:00\""
          , "\"1970-01-01T00:00:01.500Z in +00:00\"" ]
    -- ANCHOR_END: timeTest

    -- ANCHOR: randomTest
    -- 「Random：範囲の中で、同じ種なら同じ答え。shuffle は並べ替えるだけ」
    it "throws dice in the range, the same for the same seed" $
      property $ \seed (NonNegative lo) (Positive w) ->
        let world = emptyWorld { worldRandom = mkStdGen seed }
            lines' = [ "Random.int " <> num lo <> " " <> num (lo + w)
                     , "Random.shuffle [1, 2, 3, 4, 5, 6, 7]" ]
            out = map valuePart (fst (runLinesIn world lines'))
            (n, shuffled) = case out of
              [a, b] -> ( read (T.unpack a) :: Integer
                        , T.splitOn ", " (T.dropEnd 1 (T.drop 1 b)) )
              _      -> (-1, [])
        in n >= lo .&&. n <= lo + w
             .&&. sortOn id shuffled === map (T.pack . show) [1 .. 7 :: Int]
             .&&. out === map valuePart (fst (runLinesIn world lines'))
    -- ANCHOR_END: randomTest

    -- ANCHOR: filesGolden
    mapM_ (golden . ("files/" <>)) ["clock", "cleanup", "authors"]
    -- ANCHOR_END: filesGolden

  describe "big files and bytes" $ do
    -- ANCHOR: replLines
    -- 「本文の REPL の行に、本文のとおりに答える」
    it "answers the REPL lines of the chapter" $ do
      files <- mapM (\p -> (,) p <$> B.readFile (T.unpack p))
        [ "examples/streams/access.log", "examples/streams/bad.log"
        , "examples/streams/photos/lam.png" ]
      let world = emptyWorld { worldFiles = Map.fromList files }
          -- The SHA-256 of "abc" (the example of FIPS 180-2).
          -- "abc" の SHA-256（FIPS 180-2 の例）
          abc = "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f2"
                  <> "0015ad\" : String"
      fst (runLinesIn world
        [ "File.withFile \"examples/streams/access.log\" File.readLine"
        , "File.foldLines (\\n _ -> n + 1) 0 \"examples/streams/access.log\""
        , "Bytes.fromString \"ラムダ\""
        , "(length \"ラムダ\", length (Bytes.fromString \"ラムダ\"))"
        , "Bytes.toString (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Replace (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Ignore (Bytes.fromList [99, 97, 102, 233])"
        , "Bytes.decode Strict (Bytes.fromList [99, 97, 102, 233])"
        , "readFile \"examples/streams/bad.log\""
        , "File.foldLines (\\n _ -> n + 1) 0 \"examples/streams/bad.log\""
        , "File.foldLinesWith Replace (\\n _ -> n + 1) 0 "
            <> "\"examples/streams/bad.log\""
        , "Hash.sha256 (Bytes.fromString \"abc\") |> Bytes.toHex"
        , "let half = Hash.add (Bytes.fromString \"a\") Hash.start"
        , "Hash.add (Bytes.fromString \"bc\") half |> Hash.finish |> Bytes.toHex"
        , "Hash.file \"examples/streams/photos/lam.png\""
        , "Base64.encode (Bytes.fromString \"Lam!\")"
        , "Base64.decode \"TGFtIQ==\" |> Result.andThen Bytes.toString"
        , "Base64.decode \"TGFt!Q==\""
        , "Uuid.v4" ])
        `shouldBe`
          [ "Ok (Just \"2026-11-20T00:00:01Z GET /books/12 200 143ms\")"
              <> " : Result String (Maybe String)"
          , "Ok 8 : Result String Int"
          , "<9 bytes: e383a9e383a0e38380> : Bytes"
          , "(3, 9) : (Int, Int)"
          , "Err \"not UTF-8 from byte 3\" : Result String String"
          , "\"caf\65533\" : String"
          , "\"caf\" : String"
          , "error: Bytes.decode: not UTF-8 from byte 3"
          , "Err \"examples/streams/bad.log: not UTF-8 from byte 74\""
              <> " : Result String String"
          , "error: examples/streams/bad.log: line 2: not UTF-8 from byte 29"
          , "Ok 3 : Result String Int"
          , abc
          , "half : Hasher"
          , abc
          , "Ok <32 bytes: 4dcf4746ed529a95d38323c95f9e3ee6…>"
              <> " : Result String Bytes"
          , "\"TGFtIQ==\" : String"
          , "Ok \"Lam!\" : Result String String"
          , "Err \"not base64: invalid character at offset: 4\""
              <> " : Result String Bytes"
          , "\"72318d2a-0390-4070-a1ab-385bfeb0c5d6\" : String" ]
    -- ANCHOR_END: replLines

    -- ANCHOR: handleTest
    -- 「ハンドルは1行ずつ読み、閉じたら読めない。止まっても閉じる」
    it "reads line by line, closes, and closes even after a stop" $ do
      let world = emptyWorld
            { worldFiles = textFiles [("a.txt", "one\r\ntwo\nthree")] }
          (out, done) = runLinesIn world
            [ withA "mapM (\\_ -> File.readLine h) [1, 2, 3, 4]"
            , withA "do File.close h; File.readLine h"
            , withA "do _ <- File.readLine h; error \"boom\""
            , withA "mapM (\\_ -> File.readChunk 4 h) [1, 2, 3, 4, 5]"
            , "File.open \"none.txt\"" ]
          withA body = "File.withFile \"a.txt\" (\\h -> " <> body <> ")"
      map valuePart out `shouldBe`
        [ "Ok [Just \"one\", Just \"two\", Just \"three\", Nothing]"
        , "error: handle 2 is closed"
        , "error: boom"
        , "Ok [Just <4 bytes: 6f6e650d>, Just <4 bytes: 0a74776f>, "
            <> "Just <4 bytes: 0a746872>, Just <2 bytes: 6565>, Nothing]"
        , "Err \"none.txt: no such file\"" ]
      worldHandles done `shouldBe` Map.empty
    -- ANCHOR_END: handleTest

    -- ANCHOR: bytesProps
    -- 「文字 → バイト列 → 文字 で元どおり。Replace と Ignore は止まらない」
    it "turns text into bytes and back, and Replace and Ignore never stop" $
      property $ \(t, ws) ->
        let text = T.pack t
            raw  = B.pack ws
        in decodeWith Strict (encodeUtf8 text) === Right text
             .&&. isRight (decodeWith Replace raw)
             .&&. isRight (decodeWith Ignore raw)
             .&&. (isRight (decodeWith Strict raw)
                     === (decodeWith Strict raw == decodeWith Replace raw))

    -- 「少しずつのハッシュは、一度のハッシュと同じ。base64 も元に戻る」
    it "hashes a little at a time as at once, and base64 comes back" $
      property $ \(ws, cut) ->
        let list = "[" <> T.intercalate ", " (map (T.pack . show) ws) <> "]"
            n    = T.pack (show (cut `mod` (length ws + 1)))
            bs   = "(Bytes.fromList " <> list <> ")"
            end  = T.pack (show (length ws))
        in map valuePart (runLines
             [ "Hash.sha256 " <> bs <> " |> Bytes.toHex"
             , "Hash.start |> Hash.add (Bytes.slice 0 " <> n <> " " <> bs
                 <> ") |> Hash.add (Bytes.slice " <> n <> " " <> end <> " " <> bs
                 <> ") |> Hash.finish |> Bytes.toHex"
             , "Base64.decode (Base64.encode " <> bs
                 <> ") |> Result.map Bytes.toList" ])
           === [ T.pack (show (hexOf (Sha.hash (B.pack ws))))
               , T.pack (show (hexOf (Sha.hash (B.pack ws))))
               , "Ok " <> list ]
    -- ANCHOR_END: bytesProps

    -- ANCHOR: uuidTest
    -- 「UUID の7つ目は 4 で、9つ目は 8・9・a・b で始まる」
    it "makes UUIDs of version 4 and variant 1" $
      property $ \seed ->
        let world = emptyWorld { worldRandom = mkStdGen seed }
            out   = T.concat (map valuePart (fst (runLinesIn world ["Uuid.v4"])))
            parts = T.splitOn "-" (T.dropEnd 1 (T.drop 1 out))
        in map T.length parts === [8, 4, 4, 4, 12]
             .&&. T.take 1 (parts !! 2) === "4"
             .&&. T.take 1 (parts !! 3) `elem` ["8", "9", "a", "b"]
    -- ANCHOR_END: uuidTest

    -- ANCHOR: bigFile
    -- 「大きなファイル（テストの中で作る）を、本物の世界で1行ずつ数える」
    it "counts a big file of the real world, one line at a time" $ do
      dir <- getTemporaryDirectory
      -- A name of its own: the tests of other stages may run at once.
      -- 自分だけの名前：ほかの段階のテストが同時に動くかもしれない
      (path, made) <- openBinaryTempFile dir "lam-outside-big.log"
      hClose made
      let statuses = cycle ["200", "200", "304", "404", "200", "500"]
          lines' = [ "t GET / " <> s <> " 1ms" | s <- take 200000 statuses ]
      B.writeFile path (encodeUtf8 (T.unlines lines'))
      source <- Utf8.readFile "examples/streams/count.lam"
      dice   <- newDice (Just 618)
      desk   <- newDesk
      outside <- newOutside
      screen <- newIORef ""
      -- The real world, but the screen and the arguments are ours.
      -- 本物の世界。ただし画面と引数は、こちらで持つ
      let handle request = case request of
            PutStr t -> VTuple [] <$ modifyIORef' screen (<> t)
            GetArgs  -> pure (VList [VString (T.pack path)])
            _        -> realWorld dice desk outside request
      loaded <- loadProgram (diskFiles []) preludeSession preludeSession
                  "count.lam" source
      ending <- case loaded of
        Left err              -> pure (Errored err)
        Right (done, final)   -> runProgram handle done final
      removeFile path
      ending `shouldBe` Finished ""
      readIORef screen `shouldReturn`
        "200 100,001\n304 33,333\n404 33,333\n500 33,333\n"
    -- ANCHOR_END: bigFile

    -- ANCHOR: streamsGolden
    mapM_ (golden . ("streams/" <>))
      ["count", "slurp", "errors", "dupes", "inline"]
    -- ANCHOR_END: streamsGolden

  -- 「正規表現」
  describe "regular expressions" $ do
    -- ANCHOR: regexSugar
    -- 「re"..." は $regex の呼び出しに開き、中の \ はそのまま」
    it "opens re\"...\" into a call, keeping every \\" $
      map (fmap render . parseExpr)
        [ "re\"\\d{4}-\\d\\d\""
        , "Regex.find re\"a\\\"b\" s" ]
        `shouldBe` map Right
          [ "re\"\\d{4}-\\d\\d\""
          , "Regex.find re\"a\\\"b\" s" ]
    -- ANCHOR_END: regexSugar

    -- 「木の中では、ふつうの文字列を渡す、ふつうの呼び出し」
    it "is an ordinary call in the tree" $
      fmap stripLocations (parseExpr "re\"\\d+\"")
        `shouldBe` Right (App (Var "$regex") (Lit (LString "\\d+")))

    -- ANCHOR: regexErrors
    -- 「コンパイルできない正規表現は、動かす前にパーサーが見つける」
    it "finds a regex that does not compile, before running" $ do
      map (fmap (const ()) . parseExpr)
        ["re\"a(b\"", "re\"*\"", "re\"[a-\"", "re\"x{2,1}\""]
        `shouldSatisfy` all isLeft
      runLines ["re\"(\\d{4}\""]
        `shouldBe`
          [ "<repl>:1:4:\n  |\n1 | re\"(\\d{4}\"\n  |    ^\n\
            \this regex does not compile: missing closing parenthesis\n" ]
    -- ANCHOR_END: regexErrors

    -- ANCHOR: regexValues
    -- 「探す・置き換える・切り分ける」
    it "finds, replaces and splits" $
      runLines
        [ "let date = re\"(?<year>\\d{4})-(\\d\\d)-(\\d\\d)\""
        , "Regex.find date \"paid on 2026-11-21\""
        , "Regex.find date \"no date\""
        , "map .text (Regex.findAll re\"\\d+\" \"a1b22c\")"
        , "map .start (Regex.findAll re\"\\d+\" \"a1b22c\")"
        , "Regex.matches re\"^\\d+$\" \"123\""
        , "Regex.replace re\"\\d+\" \"#\" \"a1b22c\""
        , "Regex.replaceAll re\"\\d+\" \"#\" \"a1b22c\""
        , "Regex.replaceAll re\"\\d+\" \"$1\" \"a1b\""
        , "Regex.replaceWith re\"\\d+\" (\\m -> f\"<{m.text}>\") \"a1\""
        , "Regex.split re\",\\s*\" \"a, b,c\""
        , "Regex.split re\"x\" \"\""
        , "map .text (Regex.findAll re\"a*\" \"baaac\")"
        , "Regex.find re\"(?<=x)é\" \"éaxé\" |> Maybe.map .start"
        , "map .text (Regex.findAll re\"\\w+\" \"Lam は 小さな\")"
        , "map .text (Regex.findAll re\"\\d+\" \"a１２b34\")"
        , "let m = Regex.find date \"2026-11-21\" ?: "
            <> "{ text = \"\", start = 0, groups = [], named = Map.empty }"
        , "(Regex.group 2 m, Regex.group 4 m, Regex.named \"year\" m)"
        , "(Regex.group 0 m, m.groups)" ]
        `shouldBe`
          [ "date : Regex"
          , "Just { groups = [\"2026\", \"11\", \"21\"], \
            \named = {\"year\": \"2026\"}, start = 8, \
            \text = \"2026-11-21\" } : Maybe " <> matchType
          , "Nothing : Maybe " <> matchType
          , "[\"1\", \"22\"] : [String]"
          , "[1, 3] : [Int]"
          , "true : Bool"
          , "\"a#b22c\" : String"
          , "\"a#b#c\" : String"
          , "\"a$1b\" : String"
          , "\"a<1>\" : String"
          , "[\"a\", \"b\", \"c\"] : [String]"
          , "[\"\"] : [String]"
          , "[\"\", \"aaa\", \"\", \"\"] : [String]"
          , "Just 3 : Maybe Int"
          , "[\"Lam\", \"は\", \"小さな\"] : [String]"
          , "[\"１２\", \"34\"] : [String]"
          , "m : " <> matchType
          , "(Just \"11\", Nothing, Just \"2026\") : \
            \(Maybe String, Maybe String, Maybe String)"
          , "(Just \"2026-11-21\", [\"2026\", \"11\", \"21\"]) : \
            \(Maybe String, [String])" ]
    -- ANCHOR_END: regexValues

    -- ANCHOR: perlSugar
    -- 「m/…/ は re"…" に、s/…/…/ は置き換えの関数に、=~ は matches に開く」
    it "opens m/…/, s/…/…/ and =~ like Perl" $
      map (fmap render . parseExpr)
        [ "m/\\d+/i"
        , "s/\\d+/#/g"
        , "s/\\d+/#/"
        , "s/(?<y>\\d+)-(\\d+)/{2}.{y}/g"
        , "line =~ m/ERROR/"
        , "m/2"
        , "s/2/1" ]
        `shouldBe` map Right
          [ "re\"(?i)\\d+\""
          , "$re.replaceAll re\"\\d+\" \"#\""
          , "$re.replace re\"\\d+\" \"#\""
          , "$re.replaceWith re\"(?<y>\\d+)-(\\d+)\" \
            \(\\$m -> $group \"2\" $m ++ \".\" ++ $group \"y\" $m)"
          , "Regex.matchedBy line re\"ERROR\""
          , "m / 2"
          , "s / 2 / 1" ]
    -- ANCHOR_END: perlSugar

    -- ANCHOR: perlValues
    -- 「Perl 風の書き方で、探して置き換える」
    it "finds and replaces in the style of Perl" $
      runLines
        [ "Regex.find m/lam/i \"I LOVE LAM\" |> Maybe.map .text"
        , "let hide = s/\\d{2,4}-\\d{2,4}-(?<last>\\d{4})/***-****-{last}/g"
        , "hide \"call 080-9876-5432 or 03-1234-5678\""
        , "s/(\\d+)-(\\d+)/{2}-{1}/ \"11-22 33-44\""
        , "s/(\\d+)/{{{1}}}/g \"a1b2\""
        , "map s/\\s+/ /g [\"a  b\", \"c   d\"]"
        , "(\"ERROR here\" =~ m/^ERROR/, \"ok\" =~ m/^ERROR/)"
        , "Regex.matches m/a\\/b/ \"a/b\""
        , "filter (=~ m/^E/) [\"E1\", \"x\", \"E2\"]"
        , "let half m = m/2"
        , "half 10" ]
        `shouldBe`
          [ "Just \"LAM\" : Maybe String"
          , "hide : String -> String"
          , "\"call ***-****-5432 or ***-****-5678\" : String"
          , "\"22-11 33-44\" : String"
          , "\"a{1}b{2}\" : String"
          , "[\"a b\", \"c d\"] : [String]"
          , "(true, false) : (Bool, Bool)"
          , "true : Bool"
          , "[\"E1\", \"E2\"] : [String]"
          , "half : Float -> Float"
          , "5 : Float" ]
    -- ANCHOR_END: perlValues

    -- ANCHOR: perlDelimiters
    -- 「/ のかわりに ! # % でもはさめる。( や { は、いままでどおり」
    it "takes ! # and % in place of /" $
      runLines
        [ "m!/usr/bin!"
        , "s#/usr/local#/opt#g \"/usr/local/bin:/usr/local/lib\""
        , "s!a!\\!!g \"banana\""
        , "s%(\\d+)%<{1}>%g \"a1b22\""
        , "let m x = x"
        , "(m(1), m[2], m{ a = 3 })" ]
        `shouldBe`
          [ "re\"/usr/bin\" : Regex"
          , "\"/opt/bin:/opt/lib\" : String"
          , "\"b!n!n!\" : String"
          , "\"a<1>b<22>\" : String"
          , "m : a -> a"
          , "(1, [2], { a = 3 }) : (Int, [Int], { a : Int })" ]
    -- ANCHOR_END: perlDelimiters

    -- ANCHOR: perlErrors
    -- 「ないグループ、知らないフラグ、コンパイルできない正規表現は、
    --   動かす前にパーサーが見つける」
    it "finds a missing group, an unknown flag and a bad regex first" $
      runLines ["s/(?<y>\\d+)/{yy}/ \"1\"", "s/x/y/q \"x\"", "m/a(b/"]
        `shouldBe`
          [ "<repl>:1:14:\n  |\n1 | s/(?<y>\\d+)/{yy}/ \"1\"\n\
            \  |              ^\n\
            \this regex has no group yy\n"
          , "<repl>:1:7:\n  |\n1 | s/x/y/q \"x\"\n  |       ^\n\
            \unknown flag 'q' (use gimsx)\n"
          , "<repl>:1:3:\n  |\n1 | m/a(b/\n  |   ^\n\
            \this regex does not compile: missing closing parenthesis\n" ]
    -- ANCHOR_END: perlErrors

    -- ANCHOR: regexCompile
    -- 「動かしてから作る正規表現は、Result で返る。? でも開ける」
    it "compiles a regex while running, giving a Result" $
      runLines
        [ ":type Regex.compile"
        , "Regex.compile \"a(b\""
        , "Regex.compile \"a(b)\""
        , "let n = 3"
        , "Regex.compile f\"^\\\\d{{{n}}}$\""
        , "Regex.compile \"a(b\" ?: re\"x\"" ]
        `shouldBe`
          [ "String -> Result String Regex"
          , "Err \"missing closing parenthesis\" : Result String Regex"
          , "Ok re\"a(b)\" : Result String Regex"
          , "n : Number a => a\n  -- usually : Int"
          , "Ok re\"^\\d{3}$\" : Result String Regex"
          , "re\"x\" : Regex" ]
    -- ANCHOR_END: regexCompile

    -- ANCHOR: regexTypes
    -- 「Regex は比べられず、埋めこめない。関数を渡す型も確かめる」
    it "gives types to regexes and their functions" $
      runLines
        [ "re\"x\" == re\"x\""
        , "f\"{re\"x\"}\""
        , "Regex.replaceWith re\"x\" (\\m -> m.start) \"x\""
        , "let first r s = (Regex.find r s |> Maybe.map .text) ?: \"\"" ]
        `shouldBe`
          [ "type error: Regex is not in the class Comparable"
          , "type error: Regex is not in the class Display"
          , "type error: the types do not fit: Int where String is needed"
          , "first : Regex -> String -> String" ]
    -- ANCHOR_END: regexTypes

    -- ANCHOR: regexLimit
    -- 「試す道が多すぎると、PCRE2 はあきらめて、エラーにする」
    it "gives up on a regex that would try too many ways" $
      runLines
        [ "Regex.matches re\"(a+)+$\" \"" <> T.replicate 40 "a" <> "b\"" ]
        `shouldBe` ["error: Regex.matches: match limit exceeded"]
    -- ANCHOR_END: regexLimit

    -- ANCHOR: cutProps
    -- 「切れ端と見つかったものを順に並べると、もとの文字列に戻る」
    it "cuts a text into pieces that sew back into it" $
      forAll ((,) <$> elements regexes <*> genRegexText) $ \(source, t) ->
        case compileRegex source of
          Left err -> counterexample (T.unpack err) False
          Right r  -> case regexSearch r t of
            Left err -> counterexample (T.unpack err) False
            Right (Cut pieces found) ->
              let sewn = T.concat (zipLonger pieces
                           [ w | w :| _ <- found ])
              in  length pieces === length found + 1
             .&&. sewn === t
    -- ANCHOR_END: cutProps

    -- 「名前付きのグループの番号を、PCRE2 から読む」
    it "knows the numbers of the named groups" $
      fmap regexNames (compileRegex "(?<y>\\d+)-(x)-(?<z>.)")
        `shouldBe` Right [("y", 1), ("z", 3)]

    -- ANCHOR: regexGolden
    mapM_ (golden . ("regex/" <>))
      ["hide", "shoplog", "grep", "grep-bad", "grep-none"]
    -- ANCHOR_END: regexGolden

  describe "parsers" $ do
    -- ANCHOR: parserMonad
    -- 「パーサーの do も、ほかの do と同じく $do.bind に開く」
    it "opens a do of parsers into $do.bind, as any do" $
      fmap render (parseExpr "do\n  x <- p\n  q\n  pure x")
        `shouldBe` Right "$do.bind p (\\x -> $do.bind q (\\$_ -> pure x))"
    -- 「Parser は Functor・Applicative・Monad の仲間。map・pure・ap・
    --   bind が使え、ado と <*> も使える」
    it "has Parser in Functor, Applicative and Monad" $
      runLines
        [ ":type map (\\n -> n + 1) Parser.int"
        , ":type do x <- Parser.int; pure (x, x)"
        , "Parser.run (ado a <- Parser.int; Parser.string \",\";\
          \ b <- Parser.int; pure (a, b)) \"1,2\" |> toMaybe"
        , "Parser.run ((++) <$> Parser.string \"1\" <*> Parser.string \"2\")\
          \ \"12\" |> toMaybe"
        , "Parser.run (bind Parser.int (\\n -> pure (n + 1))) \"41\"\
          \ |> toMaybe"
        , "Parser.run (Parser.andThen (\\n -> pure (n + 1)) Parser.int)\
          \ \"41\" |> toMaybe"
        , "Parser.run (map (\\n -> n * 2) Parser.int) \"21\"\
          \ |> toMaybe" ]
        `shouldBe`
          [ "Parser Int"
          , "Parser (Int, Int)"
          , "Just (1, 2) : Maybe (Int, Int)"
          , "Just \"12\" : Maybe String"
          , "Just 42 : Maybe Int"
          , "Just 42 : Maybe Int"
          , "Just 42 : Maybe Int" ]
    -- ANCHOR_END: parserMonad

    -- ANCHOR: parserValues
    -- 「小さなパーサーと、その組み合わせ」
    it "reads with small parsers, put together" $
      runLines
        [ "let ok p text = Parser.run p text |> toMaybe"
        , "ok Parser.int \"42\""
        , "ok (Parser.many (Parser.string \"ab\")) \"ababab\""
        , "ok (Parser.some Parser.int) \"\""
        , "ok (Parser.sepBy Parser.int (Parser.symbol \",\")) \"1, 2,3\""
        , "ok (Parser.optional (Parser.string \"-\")) \"\""
        , "ok (Parser.oneOf [Parser.string \"ab\", Parser.string \"ac\"])\
          \ \"ac\""
        , "ok (Parser.between \"(\" \")\" (Parser.token Parser.int))\
          \ \"( 7 )\""
        , "ok (map .text (Parser.regex re\"\\d+\")) \"123\""
        , "ok (do Parser.string \"ab\"; Parser.end) \"ab\"" ]
        `shouldBe`
          [ "ok : Parser a -> String -> Maybe a"
          , "Just 42 : Maybe Int"
          , "Just [\"ab\", \"ab\", \"ab\"] : Maybe [String]"
          , "Nothing : Maybe [Int]"
          , "Just [1, 2, 3] : Maybe [Int]"
          , "Just Nothing : Maybe (Maybe String)"
          , "Just \"ac\" : Maybe String"
          , "Just 7 : Maybe Int"
          , "Just \"123\" : Maybe String"
          , "Just () : Maybe ()" ]
    -- 「Parser.regex は、その場所でしか合わない。start は全体から数える」
    it "matches a regex only right here, and counts from the whole text" $
      runLines
        [ "let p = do Parser.string \"ab\"; Parser.regex re\"\\d+\""
        , "Parser.run p \"ab12\" |> toMaybe |> Maybe.map .start"
        , "Parser.run (Parser.regex re\"\\d+\") \"x1\" |> toMaybe\
          \ |> Maybe.map .text" ]
        `shouldBe`
          [ "p : Parser " <> matchType
          , "Just 2 : Maybe Int"
          , "Nothing : Maybe String" ]
    -- ANCHOR_END: parserValues

    -- ANCHOR: parserErrors
    -- 「エラーはレコード。いちばん遠くまで読めたところを指す」
    it "gives a record, at the place that was read the farthest" $
      runLines
        [ "let at p text = Parser.run p text\
          \ |> Result.mapError (\\e -> (e.line, e.column, e.message))"
        , "at (Parser.oneOf [Parser.int,\
          \ Parser.between \"(\" \")\" Parser.int]) \"(1\""
        , "at (do Parser.string \"a\\n\"; Parser.int) \"a\\nb\""
        , "at (Parser.label \"a price\" Parser.int) \"x\""
        , "at (Parser.fail \"no way\") \"\""
        , "Parser.run Parser.int \"4x\" |> Result.mapError .pretty" ]
        `shouldBe`
          [ "at : Parser a -> String -> Result (Int, Int, String) a"
          , "Err (1, 3, \"unexpected end of input\\n\
            \expecting ')' or digit\") : Result (Int, Int, String) Int"
          , "Err (2, 1, \"unexpected 'b'\\nexpecting a number\")\
            \ : Result (Int, Int, String) Int"
          , "Err (1, 1, \"unexpected 'x'\\nexpecting a price\")\
            \ : Result (Int, Int, String) Int"
          , "Err (1, 1, \"no way\") : Result (Int, Int, String) a"
          , "Err \"1:2:\\n  |\\n1 | 4x\\n  |  ^\\nunexpected 'x'\\n\
            \expecting digit or end of input\" : Result String Int" ]
    -- 「何も読まずに合うものをくり返すと、止まらずにエラーになる」
    it "stops many with an error, if the parser reads nothing" $
      runLines [ "Parser.run (Parser.many (pure 1)) \"\"" ]
        `shouldBe`
          [ "error: Parser.many: the parser read nothing, \
            \so it would go on forever" ]
    -- 「Lam の関数のまちがいは、プログラムを止める」
    it "stops the program when a Lam function fails" $
      runLines [ "Parser.run (map (\\n -> n // 0) Parser.int) \"1\"" ]
        `shouldBe` [ "error: divided by zero" ]
    -- 「型のまちがいは、動かす前に」
    it "finds a parser of the wrong type before running" $
      runLines
        [ "Parser.run Parser.int \"1\" == Ok \"1\""
        , "Parser.many 3"
        , "do x <- Parser.int; Just x" ]
        `shouldSatisfy` all ("type error" `T.isInfixOf`)
    -- ANCHOR_END: parserErrors

    -- ANCHOR: parserLazy
    -- 「自分を指すパーサーは、Parser.lazy で」
    it "reads nested parentheses, with Parser.lazy" $
      runLines
        [ "let depth = Parser.oneOf\
          \ [ map (\\d -> d + 1)\
          \     (Parser.between \"(\" \")\" (Parser.lazy (\\_ -> depth)))\
          \ , pure 0 ]"
        , "Parser.run depth \"((()))\" |> toMaybe" ]
        `shouldBe`
          [ "depth : Number a => Parser a\n  -- usually : Parser Int"
          , "Just 3 : Maybe Int" ]
    -- ANCHOR_END: parserLazy

    -- ANCHOR: parserProps
    -- 「どんな文字のフィールドでも、引用符で囲んだ CSV から読み戻せる」
    it "reads back any fields, written as CSV in quotes" $
      withMaxSuccess 200 $ forAll (listOf1 genText) $ \fields ->
        ioProperty $ do
          let quote f = "\"" <> T.replace "\"" "\"\"" f <> "\""
              line = T.intercalate "," (map quote fields)
          answers <- replAfter "examples/parsing/csv.lam"
            ["Parser.run row " <> renderString line <> " |> toMaybe"]
          pure $ answers
            === [ "Just [" <> T.intercalate ", " (map renderString fields)
                  <> "] : Maybe [String]" ]
    -- 「電卓の答えは、Haskell で計算した答えと同じ」
    it "computes what Haskell computes" $
      withMaxSuccess 200 $ forAll genCalc $ \(text, value) -> ioProperty $ do
        answers <- replAfter "examples/parsing/calc.lam"
          ["calc " <> renderString text <> " |> toMaybe"]
        pure $ answers === ["Just " <> justArgument value <> " : Maybe Int"]
    -- ANCHOR_END: parserProps

    -- ANCHOR: parserGolden
    mapM_ (golden . ("parsing/" <>))
      ["pair", "csv", "calc", "menu", "menu-typo"]
    -- ANCHOR_END: parserGolden

  describe "decoders" $ do
    -- ANCHOR: decodeBasics
    -- 「小さな Decoder と、項目・道筋」
    it "reads with small decoders, and says where a problem is" $
      runLines
        [ ":type Decode.field"
        , "Decode.run Decode.int (JInt 42) |> toMaybe"
        , "let say d text = Decode.fromString d text\
          \ |> Result.mapError Decode.explain"
        , "say Decode.int \"\\\"42\\\"\""
        , "say (Decode.field \"cost\" Decode.int) \"{\\\"price\\\": 1}\""
        , "say (Decode.list Decode.int) \"[1, 2, \\\"three\\\", 4.5]\""
        , "say (Decode.at [\"shop\", \"tax\"] Decode.int)\
          \ \"{\\\"shop\\\": {\\\"tax\\\": \\\"10\\\"}}\""
        , "say Decode.int \"[1, 2\""
        , "(Decode.keyStep \"price\", Decode.keyStep \"unit price\")" ]
        `shouldBe`
          [ "String -> Decoder a -> Decoder a"
          , "Just 42 : Maybe Int"
          , "say : Decoder a -> String -> Result String a"
          , "Err \".: expected an Int, got \\\"42\\\"\" : Result String Int"
          , "Err \".: missing field \\\"cost\\\"\" : Result String Int"
          , "Err \".[2]: expected an Int, got \\\"three\\\"\\n\
            \.[3]: expected an Int, got 4.5\" : Result String [Int]"
          , "Err \".shop.tax: expected an Int, got \\\"10\\\"\"\
            \ : Result String Int"
          , "Err \".: not JSON: ',' or ']': not enough input\"\
            \ : Result String Int"
          , "(\".price\", \"[\\\"unit price\\\"]\") : (String, String)" ]
    -- ANCHOR_END: decodeBasics

    -- ANCHOR: decodeMonad
    -- 「Decoder は Functor・Applicative・Monad の仲間。do は最初の問題で
    --   止まり、map2 と ado は両方の問題を返す」
    it "stops a do at the first problem, and map2 and ado give both" $
      runLines
        [ ":type do x <- Decode.int; pure (x, x)"
        , "let pair = Decode.map2 (\\a b -> (a, b))\
          \ (Decode.field \"a\" Decode.int) (Decode.field \"b\" Decode.int)"
        , "let pairDo = do a <- Decode.field \"a\" Decode.int;\
          \ b <- Decode.field \"b\" Decode.int; pure (a, b)"
        , "let pairAdo = ado a <- Decode.field \"a\" Decode.int;\
          \ b <- Decode.field \"b\" Decode.int; pure (a, b)"
        , "let count d = Decode.fromString d \"{}\"\
          \ |> Result.mapError length"
        , "(count pair, count pairDo, count pairAdo)"
        , "Decode.fromString pair \"{\\\"a\\\": 1, \\\"b\\\": 2}\" |> toMaybe" ]
        `shouldBe`
          [ "Decoder (Int, Int)"
          , "pair : Decoder (Int, Int)"
          , "pairDo : Decoder (Int, Int)"
          , "pairAdo : Decoder (Int, Int)"
          , "count : Decoder a -> Result Int a"
          , "(Err 2, Err 1, Err 2) : (Result Int (Int, Int), \
            \Result Int (Int, Int), Result Int (Int, Int))"
          , "Just (1, 2) : Maybe (Int, Int)" ]
    -- ANCHOR_END: decodeMonad

    -- ANCHOR: decodeShapes
    -- 「なくてもよい項目、null、どれか1つ」
    it "reads missing fields, nulls, and one of several shapes" $
      runLines
        [ "let ok d text = Decode.fromString d text |> toMaybe"
        , "ok (Decode.optionalField \"gift\" Decode.bool) \"{}\""
        , "ok (Decode.optionalField \"gift\" Decode.bool)\
          \ \"{\\\"gift\\\": null}\""
        , "ok (Decode.nullable Decode.int) \"7\""
        , "ok (Decode.oneOf [Decode.int, map length Decode.string])\
          \ \"\\\"abc\\\"\""
        , "Decode.fromString (Decode.oneOf [Decode.int, Decode.null 0])\
          \ \"true\" |> Result.mapError length"
        , "ok (Decode.dict Decode.int) \"{\\\"a\\\": 1}\""
        , "ok (Decode.index 1 Decode.string) \"[\\\"x\\\", \\\"y\\\"]\"" ]
        `shouldBe`
          [ "ok : Decoder a -> String -> Maybe a"
          , "Just Nothing : Maybe (Maybe Bool)"
          , "Just Nothing : Maybe (Maybe Bool)"
          , "Just (Just 7) : Maybe (Maybe Int)"
          , "Just 3 : Maybe Int"
          , "Err 2 : Result Int Int"
          , "Just {\"a\": 1} : Maybe (Map String Int)"
          , "Just \"y\" : Maybe String" ]
    -- ANCHOR_END: decodeShapes

    -- ANCHOR: csvToml
    -- 「CSV は引用符と改行と \r\n を、TOML は日付と小数を読む」
    it "reads CSV with quotes, and TOML with dates and floats" $
      runLines
        [ "Csv.parse \"a,b\\n1,\\\"x, \\\"\\\"y\\\"\\\"\\\"\\n\" |> toMaybe"
        , "Csv.parse \"a,b\\n1,\\\"two\\nlines\\\"\\n\" |> toMaybe"
        , "Csv.read {n: Csv.int, x: Csv.optional Csv.number}\
          \ \"n,x\\n 7 ,\\n8,19.99\\n\" |> toMaybe"
        , "Csv.read {n: Csv.int, z: Csv.int} \"n\\nx\\n\"\
          \ |> Result.mapError Decode.explain"
        , "Toml.parse \"when = 2026-11-23T09:00:00+09:00\\nratio = 0.25\\n\""
        , "Toml.parse \"a = 1\\na = 2\\n\"" ]
        `shouldBe`
          [ "Just [{\"a\": \"1\", \"b\": \"x, \\\"y\\\"\"}]\
            \ : Maybe [Map String String]"
          , "Just [{\"a\": \"1\", \"b\": \"two\\nlines\"}]\
            \ : Maybe [Map String String]"
          , "Just (JArr [JObj {\"n\": JInt 7, \"x\": JNull},\
            \ JObj {\"n\": JInt 8, \"x\": JDec 1999 2}]) : Maybe Json"
          , "Err \".: missing column \\\"z\\\"\" : Result String Json"
          , "Ok (JObj {\"ratio\": JDec 25 2,\
            \ \"when\": JStr \"2026-11-23T09:00:00+09:00\"})\
            \ : Result String Json"
          , "Err \"2:1: key error: a is already assigned\"\
            \ : Result String Json" ]
    -- ANCHOR_END: csvToml

    -- ANCHOR: fromJsonTests
    -- 「型が Decoder を選ぶ」
    it "chooses the decoder by the type" $ do
      answers <- replAfter "examples/decode/fromjson.lam"
        [ ":type fromJson"
        , "fromJson \"[\\\"a\\\", \\\"b\\\"]\" |> toMaybe\
          \ |> Maybe.map (join \"+\")" ]
      answers `shouldBe`
        [ "FromJson a => String -> Result [{ message : String,\
          \ path : String }] a"
        , "Just \"a+b\" : Maybe String" ]
    -- ANCHOR_END: fromJsonTests

    -- ANCHOR: decodeProps
    -- 「Int でないものの数だけ、問題が返ってくる」
    it "gives one problem for every item that is not an Int" $
      withMaxSuccess 200 $ forAll (listOf genIntOrText) $ \items ->
        ioProperty $ do
          let json = "[" <> T.intercalate ", " (map itemJson items) <> "]"
              bad  = length [ () | Right _ <- items ]
              want | bad == 0  = "Ok " <> T.pack (show (length items))
                   | otherwise = "Err " <> T.pack (show bad)
          answers <- replAfter "examples/decode/try.lam"
            [ "Decode.fromString (Decode.list Decode.int) "
                <> renderString json
                <> " |> Result.map length |> Result.mapError length" ]
          pure $ answers === [want <> " : Result Int Int"]
    -- 「どんなフィールドでも、CSV に書けば Csv.parse で読み戻せる」
    it "reads back any fields, written as CSV" $
      withMaxSuccess 200 $ forAll genTable $ \(header, rows) ->
        ioProperty $ do
          let quote f = "\"" <> T.replace "\"" "\"\"" f <> "\""
              line fs = T.intercalate "," (map quote fs)
              csv = T.intercalate "\r\n" (map line (header : rows))
              shown row = "{" <> T.intercalate ", "
                [ renderString k <> ": " <> renderString v
                | (k, v) <- Map.toList (Map.fromList (zip header row)) ]
                <> "}"
          answers <- replAfter "examples/decode/try.lam"
            ["Csv.parse " <> renderString csv <> " |> toMaybe"]
          pure $ answers
            === [ "Just [" <> T.intercalate ", " (map shown rows)
                  <> "] : Maybe [Map String String]" ]
    -- ANCHOR_END: decodeProps

    -- ANCHOR: decodeGolden
    mapM_ (golden . ("decode/" <>))
      [ "try", "oldway", "orders", "orders-bad", "check", "payments"
      , "fromjson", "register", "config", "sales", "sales-bad" ]
    -- ANCHOR_END: decodeGolden

  -- 「外の世界」
  describe "outside" $ do
    -- ANCHOR: outsideGolden
    mapM_ (golden . ("outside/" <>)) ["weather", "half", "dex"]
    -- ANCHOR_END: outsideGolden

    -- ANCHOR: weatherTwice
    -- 「次の日に動かすと、表が育ち、変わったところを知らせる」
    it "keeps the days, and tells what changed on the next run" $ do
      dbs  <- memoryDatabases
      next <- Utf8.readFile "examples/outside/web/tokyo-next.json"
      _ <- runExampleWith dbs id "examples/outside/weather.lam"
      (out, _) <- runExampleWith dbs
        (\w -> w { worldWeb = Map.map (const (200, next)) (worldWeb w) })
        "examples/outside/weather.lam"
      T.lines out `shouldContain`
        [ "2026-11-20  rain  30%  max  15.3  min   7.1  rain 4% -> 30%"
        , "2026-11-21  rain   2%  max  14.6  min   4.5  max 13.1 -> 14.6" ]
      T.lines out `shouldContain`
        [ "2026-11-24  rain   1%  max  12.4  min   3.0  new" ]
      -- 11-18 has no max (null) in the next forecast: the old 20.4 is
      -- kept, and it is not a change.
      -- 次の予報には 11-18 の最高がない（null）：前の 20.4 を残し、
      -- 変化とは言わない
      T.lines out `shouldContain`
        [ "2026-11-18  rain 100%  max  20.4  min   9.5" ]
      last (T.lines out) `shouldBe` "kept 8 days, the hottest 20.4"
    -- ANCHOR_END: weatherTwice

    -- ANCHOR: halfWeek
    -- 「1日ずつ貯めると、途中で失敗したときに半分の週が残る。
    --   トランザクションなら、何も残らない」
    it "keeps half a week without a transaction, and none with one" $ do
      odd' <- Utf8.readFile "examples/outside/web/tokyo-odd.json"
      let oddWeb w = w { worldWeb = Map.map (const (200, odd')) (worldWeb w) }
          count dbs = do
            (_, after) <- scriptIO dbs emptyWorld $ T.unlines
              [ "main = do"
              , "  r <- Sql.withDb \"weather.db\" (\\db ->"
              , "         Sql.query db"
              , "           \"SELECT count(*) AS n FROM weather\" [])"
              , "  putStrLn (show r)" ]
            pure (outputOf after)
      half <- memoryDatabases
      (out1, _) <- runExampleWith half id "examples/outside/half.lam"
      count half `shouldReturn` "Ok [{\"n\": JInt 3}]\n"
      whole <- memoryDatabases
      (out2, _) <- runExampleWith whole oddWeb "examples/outside/weather.lam"
      count whole `shouldReturn` "Ok [{\"n\": JInt 0}]\n"
      -- Both stop at the same day, with the same error.
      -- どちらも同じ日で、同じエラーで止まる
      last (T.lines out1) `shouldBe` last (T.lines out2)
    -- ANCHOR_END: halfWeek

    -- ANCHOR: dexTwice
    -- 「まだ持っていないポケモンだけを、Web に聞く」
    it "asks the web only for a Pokemon it does not have yet" $ do
      dbs <- memoryDatabases
      (_, first)  <- runExampleWith dbs id "examples/outside/dex.lam"
      (_, second) <- runExampleWith dbs id "examples/outside/dex.lam"
      reverse (worldSent first) `shouldBe`
        [ "GET https://pokeapi.co/api/v2/pokemon/pikachu"
        , "GET https://pokeapi.co/api/v2/pokemon-species/pikachu"
        , "GET https://pokeapi.co/api/v2/pokemon/nosuchmon" ]
      -- The second run: Pikachu is in dex.db, so only the missing one.
      -- 2回目：ピカチュウは dex.db にあるので、ないものだけ
      reverse (worldSent second) `shouldBe`
        [ "GET https://pokeapi.co/api/v2/pokemon/nosuchmon" ]
    -- ANCHOR_END: dexTwice

    -- ANCHOR: readers
    -- 「読み方を選べる。Err は、答えがない・状態が悪い・本文が読めない」
    it "reads an answer the way it is told, and says why it failed" $ do
      dbs <- memoryDatabases
      let world = emptyWorld { worldWeb = Map.fromList
            [ ("https://x.test/ok",  (200, "{\"n\": 1}"))
            , ("https://x.test/bad", (200, "{\"n\": "))
            , ("https://x.test/404", (404, "{\"error\": \"no\"}")) ] }
      (ending, after) <- scriptIO dbs world $ T.unlines
        [ "n = Decode.field \"n\" Decode.int"
        , "main = do"
        , "  a <- Http.get \"https://x.test/ok\""
        , "  b <- Http.get \"https://x.test/bad\""
        , "  c <- Http.get \"https://x.test/404\""
        , "  d <- Http.get \"https://x.test/none\""
        , "  e <- Http.request Http.string Http.json \"GET\""
        , "         \"https://x.test/404\" {} \"\""
        , "  f <- Http.request Http.response Http.string \"GET\""
        , "         \"https://x.test/ok\" {} \"\""
        , "  g <- Http.getAs n \"https://x.test/ok\""
        , "  h <- Http.getAs (Decode.field \"m\" Decode.int)"
        , "         \"https://x.test/ok\""
        , "  putStrLn (show (a, c, d))"
        , "  putStrLn (show (e, Result.map .status f))"
        , "  putStrLn (show (g, h))"
        , "  putStrLn (show b)" ]
      ending `shouldBe` Finished ""
      outputOf after `shouldBe` T.unlines
        [ "(Ok (JObj {\"n\": JInt 1}), "
            <> "Err \"status 404: {\\\"error\\\": \\\"no\\\"}\", "
            <> "Err \"https://x.test/none: cannot connect\")"
        , "(Err (BadStatus 404 (JObj {\"error\": JStr \"no\"})), Ok 200)"
        , "(Ok 1, Err \".: missing field \\\"m\\\"\")"
        , "Err \"the answer is not JSON (no content-type; "
            <> "it begins with \\\"{\\\\\\\"n\\\\\\\": \\\")\"" ]
    -- ANCHOR_END: readers

    -- ANCHOR: queryAsTest
    -- 「行も Decoder で読む。読めない行は、行の番号つきで全部言う」
    it "reads the rows with a decoder, and tells every row that is wrong" $ do
      dbs <- memoryDatabases
      (ending, after) <- scriptIO dbs emptyWorld $ T.unlines
        [ "pt = Decode.map2 (\\x n -> (x, n))"
        , "       (Decode.field \"x\" Decode.int)"
        , "       (Decode.field \"n\" Decode.string)"
        , "main = do"
        , "  r <- Sql.withDb \":memory:\" (\\db -> do"
        , "    Sql.exec db \"CREATE TABLE t (x, n)\" [] ?"
        , "    Sql.exec db"
        , "      \"INSERT INTO t VALUES (1, 'a'), ('b', 2), (3, 'c')\" [] ?"
        , "    good <- Sql.queryAs pt db \"SELECT * FROM t WHERE x = 1\" [] ?"
        , "    putStrLn (show good)"
        , "    Sql.queryAs pt db \"SELECT * FROM t\" [])"
        , "  putStrLn (show r)" ]
      ending `shouldBe` Finished ""
      outputOf after `shouldBe` T.unlines
        [ "[(1, \"a\")]"
        , "Err \".[1].x: expected an Int, got \\\"b\\\"\\n"
            <> ".[1].n: expected a String, got 2\"" ]
    -- ANCHOR_END: queryAsTest

    -- ANCHOR: transactionTest
    -- 「トランザクション：Ok なら全部、Err か止まったら何も残さない」
    it "keeps all of a transaction, or none of it" $ do
      dbs <- memoryDatabases
      (ending, after) <- scriptIO dbs emptyWorld $ T.unlines
        [ "add db x = Sql.exec db \"INSERT INTO t VALUES (?)\" [JInt x]"
        , "count db = Sql.query db \"SELECT count(*) AS n FROM t\" []"
        , "main = do"
        , "  r <- Sql.withDb \"t.db\" (\\db -> do"
        , "    Sql.exec db \"CREATE TABLE t (x)\" [] ?"
        , "    a <- Sql.transaction db (do"
        , "      add db 1 ?"
        , "      add db 2)"
        , "    b <- Sql.transaction db (do"
        , "      add db 3 ?"
        , "      pure (Err \"no\"))"
        , "    c <- Sql.transaction db (do"
        , "      add db 4 ?"
        , "      error \"stop\")"
        , "    n <- count db"
        , "    pure (Ok (a, b, c, n)))"
        , "  putStrLn (show r)" ]
      ending `shouldBe` Finished ""
      -- 1 and 2 are kept; 3 (Err) and 4 (a stop) are rolled back.
      -- 1 と 2 は残る。3（Err）と 4（止まった）は、なかったことに
      outputOf after `shouldBe`
        "Ok (Ok 1, Err \"no\", Err \"stop\", Ok [{\"n\": JInt 2}])\n"
    -- ANCHOR_END: transactionTest

    -- ANCHOR: cellsTest
    -- 「整数・短い小数・文字列・null は、貯めて読むと元のまま」
    it "gives back an integer, a short decimal, a text and null as it was" $
      property $ forAll genCell $ \(cell, shown) -> ioProperty $ do
        dbs <- memoryDatabases
        let ask r = fromMaybe (error "not SQL") (sqlRequest dbs r)
        _ <- ask (SqlOpen "t.db" ReadWrite)
        _ <- ask (SqlExec 1 "CREATE TABLE t (x)" [])
        _ <- ask (SqlExec 1 "INSERT INTO t VALUES (?)" [cell])
        rows <- ask (SqlQuery 1 "SELECT x FROM t" [])
        pure (renderValue rows === "Ok [{\"x\": " <> shown <> "}]")
    it "says no to an integer SQLite cannot keep, and to an array" $ do
      dbs <- memoryDatabases
      (ending, after) <- scriptIO dbs emptyWorld $ T.unlines
        [ "big = 9223372036854775807"
        , "main = do"
        , "  r <- Sql.withDb \":memory:\" (\\db -> do"
        , "    a <- Sql.query db \"SELECT ? AS n\" [JInt big]"
        , "    b <- Sql.query db \"SELECT ? AS n\" [JInt (big + 1)]"
        , "    c <- Sql.query db \"SELECT ? AS n\" [JArr []]"
        , "    d <- Sql.exec db \"CREATE TABLE t (x)\" []"
        , "    pure (Ok (a, b, c, d)))"
        , "  putStrLn (show r)" ]
      ending `shouldBe` Finished ""
      outputOf after `shouldBe` T.unlines
        [ "Ok (Ok [{\"n\": JInt 9223372036854775807}], "
            <> "Err \"Sql: the integer is too large\", "
            <> "Err \"Sql: an array or an object cannot be a cell; "
            <> "use Json.show\", "
            <> "Ok 0)" ]
    -- ANCHOR_END: cellsTest

    -- ANCHOR: realFile
    -- 「本物のファイル：読むだけに開くと書けず、ないファイルは作らない」
    it "opens a real file to read only, and makes no missing file" $ do
      tmp <- getTemporaryDirectory
      (path, h) <- openTempFile tmp "lam-outside.db"
      hClose h
      dbs <- realDatabases
      let ask r = fromMaybe (error "not SQL") (sqlRequest dbs r)
          file = T.pack path
      _ <- ask (SqlOpen file ReadWrite)
      _ <- ask (SqlExec 1 "CREATE TABLE t (x)" [])
      _ <- ask (SqlExec 1 "INSERT INTO t VALUES (?)" [CInt 7])
      _ <- ask (SqlClose 1)
      _ <- ask (SqlOpen file ReadOnly)
      written <- ask (SqlExec 2 "INSERT INTO t VALUES (8)" [])
      rows    <- ask (SqlQuery 2 "SELECT x FROM t" [])
      missing <- ask (SqlOpen (file <> "-missing") ReadOnly)
      closeDatabases dbs
      made <- doesFileExist (path <> "-missing")
      removeFile path
      renderValue written `shouldSatisfy` ("Err " `T.isPrefixOf`)
      renderValue rows `shouldBe` "Ok [{\"x\": JInt 7}]"
      renderValue missing `shouldSatisfy` ("Err " `T.isPrefixOf`)
      made `shouldBe` False
    -- ANCHOR_END: realFile

  -- 「鍵をかける」
  describe "guards" $ do
    -- ANCHOR: guardGolden
    mapM_ (golden . ("guard/" <>)) ["peek", "sneaky"]
    -- 「HOME は見えるが、秘密のトークンは見えない」
    it "examples/guard/env.lam" $ do
      dbs <- memoryDatabases
      let token w = w { worldEnv = Map.insert "GITHUB_TOKEN" "abc"
                                     (worldEnv w) }
      (out, _) <- runExampleWith dbs token "examples/guard/env.lam"
      expected <- Utf8.readFile "examples/guard/env.out"
      out `shouldBe` expected
    -- ANCHOR_END: guardGolden

    -- ANCHOR: guardProp
    -- 「権限がなければ、ハンドラーに頼まずに答える」
    it "asks the handler only when every permission is given" $
      property $ forAll (sublistOf [minBound .. maxBound]) $ \given ->
        forAll (elements sampleRequests) $ \request ->
          let perms = Only (Map.fromList [ (p, Everything) | p <- given ])
              asked = snd (runState (allowOnly perms counting request) 0)
              counting _ = modify (+ (1 :: Int)) >> pure (VTuple [])
              needed = map fst (needs request)
          in asked == (if all (`elem` given) needed then 1 else 0)
    -- ANCHOR_END: guardProp

    -- ANCHOR: scopes
    -- 「ホスト・フォルダー・名前・コマンドを指して許す」
    it "allows only the hosts, folders, names and commands it is given" $ do
      covers Net (Within ["pokeapi.co"]) (hostOf "https://pokeapi.co/api/v2/")
        `shouldBe` True
      -- The host is pokeapi.co.evil.test, not pokeapi.co.
      -- ホストは pokeapi.co.evil.test で、pokeapi.co ではない
      covers Net (Within ["pokeapi.co"])
        (hostOf "https://pokeapi.co.evil.test/x") `shouldBe` False
      let reads = covers Read (Within ["data"])
      map reads ["data/a.txt", "./data/b/c.txt", "data2/a.txt"
                , "data/../secret.txt", "secret.txt"]
        `shouldBe` [True, True, False, False, False]
      map (covers Write (Within ["."])) ["dex.db", "out/a.txt", "../x.db"]
        `shouldBe` [True, True, False]
      covers Env (Within ["HOME"]) "HOME" `shouldBe` True
      covers Env (Within ["HOME"]) "AWS_SECRET" `shouldBe` False
      covers Run (Within ["git"]) "git" `shouldBe` True
      covers Run (Within ["git"]) "/usr/bin/git" `shouldBe` False
      map globRoot ["logs/*.log", "logs/2026-*/*.log", "*.txt", "a/b.txt"]
        `shouldBe` ["logs", "logs", ".", "a/b.txt"]
    it "hides the environment variables that are not allowed" $ do
      dbs <- memoryDatabases
      let world = emptyWorld { worldEnv = Map.fromList
                    [("HOME", "/home/lam"), ("TOKEN", "s3cret")] }
          source = T.unlines
            [ "main = do"
            , "  h <- lookupEnv \"HOME\""
            , "  t <- lookupEnv \"TOKEN\""
            , "  e <- getEnvironment"
            , "  putStrLn (show (h, t, Map.keys e))" ]
          run flags = outputOf . snd <$> scriptFlagged dbs flags world source
      run [] `shouldReturn`
        "(Just \"/home/lam\", Just \"s3cret\", [\"HOME\", \"TOKEN\"])\n"
      run ["--allow-env=HOME"] `shouldReturn`
        "(Just \"/home/lam\", Nothing, [\"HOME\"])\n"
      run ["--deny-all"] `shouldReturn` "(Nothing, Nothing, [])\n"
    -- 「許されない File.exists は false、コマンドは始まらない」
    it "says false for a file it may not look at, and starts no command" $ do
      dbs <- memoryDatabases
      let world = emptyWorld
            { worldFiles = Map.fromList [("a.txt", "a")]
            , worldCommands = Map.fromList [(["rm", "a.txt"], (0, "", ""))] }
          source = T.unlines
            [ "main = do"
            , "  seen <- File.exists \"a.txt\""
            , "  rm <- Process.run \"rm\" [\"a.txt\"]"
            , "  putStrLn (show seen)"
            , "  putStrLn (show rm)" ]
      (_, after) <- scriptFlagged dbs ["--allow-run=git"] world source
      outputOf after `shouldBe` T.unlines
        [ "false", "CannotStart \"rm: needs --allow-run=rm\"" ]
    -- ANCHOR_END: scopes

    -- ANCHOR: runGolden
    -- 「--allow-run=git なら git だけ。--dry-run では git も動かさない」
    it "runs git only with --allow-run=git, and not on a dry run" $ do
      let authors flags = do
            dbs <- memoryDatabases
            fst <$> runFlagged dbs id flags "examples/files/authors.lam"
      expected <- Utf8.readFile "examples/files/authors.out"
      authors ["--allow-run=git"] `shouldReturn` expected
      authors ["--allow-run=git,ls"] `shouldReturn` expected
      authors ["--deny-all"] `shouldReturn`
        "git: needs --allow-run=git\n[exit 1]\n"
      authors ["--allow-run=ls"] `shouldReturn`
        "git: needs --allow-run=git\n[exit 1]\n"
      authors ["--allow-run=git", "--dry-run"] `shouldReturn` T.unlines
        [ "dry-run: Process.run \"git\" [\"log\", \"--format=%an\"]"
        , "dry run: git was not run", "[exit 1]" ]
    -- ANCHOR_END: runGolden

    -- ANCHOR: flags
    -- 「ファイルより前のフラグを読み、残りはスクリプトのもの」
    it "reads the flags before the file, and leaves the rest" $ do
      readOptions [] Nothing `shouldBe` Right (defaultOptions, [])
      readOptions ["--allow-net", "--trace", "a.lam", "--allow-read"] Nothing
        `shouldBe` Right
          ( defaultOptions
              { permissions = Only (Map.fromList [(Net, Everything)])
              , tracing = True }
          , ["a.lam", "--allow-read"] )
      let perms flags = fmap (permissions . fst) (readOptions flags Nothing)
      perms ["--deny-all"] `shouldBe` Right (Only Map.empty)
      perms ["--allow-read", "--deny-all"]
        `shouldBe` Right (Only (Map.fromList [(Read, Everything)]))
      perms ["--allow-read=data,logs", "--allow-run=git"]
        `shouldBe` Right (Only (Map.fromList
          [(Read, Within ["data", "logs"]), (Run, Within ["git"])]))
      perms ["--allow-read=data", "--allow-read"]
        `shouldBe` Right (Only (Map.fromList [(Read, Everything)]))
      readOptions ["--alow-net", "a.lam"] Nothing
        `shouldBe` Left "lam: unknown flag: --alow-net"
      readOptions ["--allow-time", "a.lam"] Nothing `shouldSatisfy` isLeft
    -- 「種は --seed=618 が先、なければ LAM_SEED」
    it "takes the seed from --seed first, then from LAM_SEED" $ do
      let seed flags env = fmap (randomSeed . fst) (readOptions flags env)
      seed ["--seed=618"] (Just "1") `shouldBe` Right (Just 618)
      seed [] (Just "618") `shouldBe` Right (Just 618)
      seed [] Nothing `shouldBe` Right Nothing
      seed ["--seed=x"] Nothing
        `shouldBe` Left "lam: --seed needs a whole number: x"
    -- ANCHOR_END: flags

    -- ANCHOR: traceTest
    -- 「許されない頼みごとも、trace には出る。1行ずつ読むのは数だけ」
    it "traces every request, even one that is not allowed" $ do
      dbs <- memoryDatabases
      let world = emptyWorld
            { worldFiles = Map.fromList [("log.txt", "a\nb\nc\n")] }
      (_, after) <- scriptFlagged dbs ["--trace", "--allow-read=log.txt"]
        world $ T.unlines
          [ "main = do"
          , "  a <- readFile \"メモ.txt\""
          , "  b <- Http.get \"https://x.test/\""
          , "  n <- File.foldLines (\\k _ -> k + 1) 0 \"log.txt\""
          , "  putStrLn (show (a, b, n))" ]
      errorsOf after `shouldBe` T.unlines
        [ "trace 0.000s: readFile \"メモ.txt\""
        , "trace 0.000s: Http.send \"GET\" \"https://x.test/\""
        , "trace 0.000s: File.open \"log.txt\" -> #1"
        , "trace 0.000s: File.close #1 (4 reads)" ]
      worldSent after `shouldBe` []
    it "traces dex.lam: the cache, then the web" $ do
      dbs <- memoryDatabases
      (out, _) <- runFlagged dbs id ["--trace"] "examples/outside/dex.lam"
      expected <- Utf8.readFile "examples/guard/dex-trace.out"
      out `shouldBe` expected
    -- ANCHOR_END: traceTest

    -- ANCHOR: dryRunTest
    -- 「--dry-run は、変えずに、何をするはずだったかを知らせる」
    it "changes nothing with --dry-run, but tells what it would do" $ do
      dbs <- memoryDatabases
      let world = emptyWorld
            { worldWeb = Map.fromList [("https://x.test/n", (200, "{}"))]
            , worldFiles = Map.fromList [("old.txt", "old")] }
      _ <- scriptFlagged dbs [] world $ T.unlines
        [ "main : IO (Result String ())"
        , "main = do"
        , "  db <- Sql.open \"d.db\" ?"
        , "  Sql.exec db \"CREATE TABLE t (x)\" [] ?"
        , "  pure (Ok ())" ]
      (_, after) <- scriptFlagged dbs ["--dry-run"] world $ T.unlines
        [ "main : IO (Result String ())"
        , "main = do"
        , "  w <- writeFile \"out.txt\" \"hi\""
        , "  r <- File.remove \"old.txt\""
        , "  p <- Http.post \"https://x.test/n\" JNull"
        , "  db <- Sql.open \"d.db\" ?"
        , "  n <- Sql.transaction db"
        , "         (Sql.exec db \"INSERT INTO t VALUES (1)\" [])"
        , "  rows <- Sql.query db \"SELECT * FROM t\" []"
        , "  putStrLn (show (w, r, p, n, rows))"
        , "  pure (Ok ())" ]
      worldFiles after `shouldBe` Map.fromList [("old.txt", "old")]
      worldSent after `shouldBe` []
      outputOf after `shouldBe`
        ("(Ok (), Ok (), "
           <> "Err \"dry run: POST https://x.test/n was not sent\", "
           <> "Ok 0, Ok [])\n")
      errorsOf after `shouldBe` T.unlines
        [ "dry-run: writeFile \"out.txt\" (2 letters)"
        , "dry-run: File.remove \"old.txt\""
        , "dry-run: Http.send \"POST\" \"https://x.test/n\""
        , "dry-run: Sql.exec #2 \"BEGIN\" []"
        , "dry-run: Sql.exec #2 \"INSERT INTO t VALUES (1)\" []"
        , "dry-run: Sql.exec #2 \"COMMIT\" []" ]
    -- 「仕込み忘れた File.remove も、lam の --dry-run なら止まる」
    it "keeps the logs of cleanup.lam with lam --dry-run" $ do
      dbs <- memoryDatabases
      (out, after) <- runFlagged dbs id ["--dry-run"]
                        "examples/files/cleanup.lam"
      expected <- Utf8.readFile "examples/guard/cleanup-dry.out"
      out `shouldBe` expected
      Map.keys (worldFiles after) `shouldSatisfy`
        elem "examples/files/logs/app-2026-11-01.log"
    -- ANCHOR_END: dryRunTest

  -- 「型から作るコマンドライン」
  describe "command line" $ do
    -- ANCHOR: cliGolden
    -- 「本文の例：偽物の世界で動かしても、本物の lam と同じ出力」
    cmds <- runIO (listDirectory "examples/cli")
    mapM_ (cliGolden . dropExtension)
      (sortOn id [ c | c <- cmds, takeExtension c == ".cmd" ])
    -- ANCHOR_END: cliGolden

    -- ANCHOR: cliKebab
    -- 「dryRun は --dry-run、max_count は --max-count」
    it "turns camelCase into kebab-case" $
      map kebab ["dryRun", "max_count", "name"]
        `shouldBe` ["dry-run", "max-count", "name"]
    -- ANCHOR_END: cliKebab

    -- ANCHOR: cliParse
    -- 「レコードの型は、ヘルプの行（---）・既定値・短い名前を持つ」
    it "keeps the help, the defaults and the short names" $
      fmap (\(AliasDecl _ _ te) -> te)
        (runLamParser ops aliasDecl "" (T.unlines
          [ "type alias O ="
          , "  { --- Who"
          , "    -- だれ"
          , "    name : String"
          , "    --- How many"
          , "    --- (at least 1)"
          , "    ----------"
          , "  , count : Int = 1"
          , "  , alias n = count"
          , "  , tags : [String] = []"
          , "  , level : Level = Normal"
          , "  }" ]))
        `shouldBe` Right (TyRecord
          [ FieldType "name" (TyCon "String" []) Nothing ["Who"] []
          , FieldType "count" (TyCon "Int" [])
              (Just (DefaultOne (LInt 1))) ["How many", "(at least 1)"] ["n"]
          , FieldType "tags" (TyList (TyCon "String" []))
              (Just (DefaultList [])) [] []
          , FieldType "level" (TyCon "Level" [])
              (Just (DefaultCon "Normal")) [] [] ]
          Nothing)
    -- ANCHOR_END: cliParse

    -- ANCHOR: cliHelpLine
    -- 「ヘルプになるのは --- の行だけ。-- と ---- はならない」
    it "takes only --- lines as help" $
      map helpLine
        ["--- Who to greet", "  ---", "-- あいさつする相手", "----", "x"]
        `shouldBe` [Just "Who to greet", Just "", Nothing, Nothing, Nothing]
    -- ANCHOR_END: cliHelpLine

    -- ANCHOR: cliProps
    -- 「--count n で、n 回あいさつする」
    it "reads --count as a whole number" $
      forAll (choose (0, 5)) $ \n -> ioProperty $ do
        out <- cliRun "examples/cli/greet" ["Lam", "--count", T.pack (show n)]
        pure (out === T.replicate n "Hello, Lam!\n")
    -- 「値を取るオプションの値は、- で始まってもよい」
    it "takes a value that starts with -" $ do
      out <- cliRun "examples/cli/bill" ["100", "--tip", "-0.1", "-p", "3"]
      out `shouldBe` "each pays 30.00\n"
    -- 「-- の後ろは、オプションに見えても引数」
    it "reads the words after -- as arguments" $ do
      out <- cliRun "examples/cli/greet" ["--", "--loud"]
      out `shouldBe` "Hello, --loud!\n"
    -- 「--count=2 とも書ける。何度も書けば、最後のもの」
    it "reads --count=2, and the last of many" $ do
      out <- cliRun "examples/cli/greet" ["--count=1", "Lam", "-n", "2"]
      out `shouldBe` "Hello, Lam!\nHello, Lam!\n"
    -- ANCHOR_END: cliProps

    -- ANCHOR: cliFromArg
    -- 「FromArg のインスタンスで読む：Int の読めた数は、そのまま戻る」
    it "reads any whole number with the instance of Int" $
      property $ \(n :: Integer) ->
        let src = "main : { n : Int } -> IO ()\nmain o = putStrLn (show o.n)"
        in outputOf (snd (runEnding noFiles
             emptyWorld { worldArgs = ["--", T.pack (show n)] } "m.lam" src))
             === T.pack (show n) <> "\n"
    -- 「自分の型のインスタンスで読む。大文字でも読める」
    it "reads a Level with its own instance" $ do
      out <- cliRun "examples/cli/remind"
               ["x", "2026-11-27T09:00:00Z", "--level=Quiet"]
      out `shouldBe` "(x at 2026-11-27T09:00:00Z)\n"
    -- ANCHOR_END: cliFromArg

    -- ANCHOR: cliResult
    -- 「main が IO (Result e ()) なら、Err は error: で止まる」
    it "stops at an Err of main, as the script chapter does" $
      fst (runEnding noFiles emptyWorld { worldArgs = ["Lam"] } "m.lam"
        "main : { name : String } -> IO (Result String ())\n\
        \main o = pure (Err f\"no {o.name}\")")
        `shouldBe` Errored "error: no Lam"
    -- ANCHOR_END: cliResult

    -- ANCHOR: cliPlain
    -- 「注釈がなければ、フィールドは名前の順の引数」
    it "reads the fields in the order of their names without an annotation" $
      outputOf (snd (runEnding noFiles emptyWorld { worldArgs = ["1", "2"] }
        "m.lam" "main opts = putStrLn (opts.b ++ opts.a)"))
        `shouldBe` "21\n"
    -- ANCHOR_END: cliPlain

    -- ANCHOR: cliTodo
    -- 「サブコマンド：足して、済ませて、見る」
    it "runs the subcommands of todo.lam one after another" $ do
      source <- Utf8.readFile "examples/cli/todo.lam"
      let run world ws = runEnding noFiles
            world { worldArgs = ws, worldOutput = [] } "todo.lam" source
          (_, w1) = run emptyWorld ["add", "Buy milk", "-t", "shop"]
          (_, w2) = run w1 ["add", "Write"]
          (_, w3) = run w2 ["done", "2"]
          (_, w4) = run w3 ["list"]
      outputOf w4 `shouldBe` "1. [ ] Buy milk #shop\n2. [x] Write\n"
      -- 「list は何も受け取らない」
      fst (run w4 ["list", "extra"]) `shouldBe` ExitedWith 2
    -- ANCHOR_END: cliTodo

  -- 「シェルのように」
  describe "like a shell" $ do
    -- ANCHOR: shellParse
    -- 「$"…" は、Shell.command に語のリストを渡す呼び出しに開く」
    it "opens $\"...\" into a call of the hidden command" $
      parseExpr "$\"git branch -d {name}\"" `shouldBe` Right
        (App (Var "$sh.command")
          (BinOp Concat (List [Lit (LString "git")])
            (BinOp Concat (List [Lit (LString "branch")])
              (BinOp Concat (List [Lit (LString "-d")])
                (App (Var "$sh.args") (Var "name"))))))
    -- 「くっつけた語と、'…' の語」
    it "glues letters and fields, and keeps '...' in one word" $
      parseExpr "$\"x --tag={t} 'a b'\"" `shouldBe` Right
        (App (Var "$sh.command")
          (BinOp Concat (List [Lit (LString "x")])
            (BinOp Concat
              (App (Var "$sh.glue")
                (List [ List [Lit (LString "--tag=")]
                      , App (Var "$sh.args") (Var "t") ]))
              (List [Lit (LString "a b")]))))
    -- 「名前のないコマンドは、動かす前に止める」
    it "wants a name" $
      parseExpr "$\"  \"" `shouldSatisfy` isLeft
    -- ANCHOR_END: shellParse

    -- ANCHOR: shellWords
    -- 「どんな値も、1つの語のまま、コマンドに届く」
    it "gives any value as one word, as it is" $
      property $ forAll genText $ \v ->
        let source = "main = do\n  _ <- Shell.run $\"printf %s {"
                       <> renderString v <> "}\"\n  pure ()"
            (_, w) = script emptyWorld source
        in map commandArgs (worldRan w) === [["%s", v]]
    -- 「Shell.render の書き方は、$"…" に読み戻せる」
    it "renders a word that $\"...\" reads back" $
      property $ forAll genText $ \v ->
        let source = "main = do\n  _ <- Shell.run $\"echo "
                       <> quoteWord v <> "\"\n  pure ()"
            (_, w) = script emptyWorld source
        in map commandArgs (worldRan w) === [[v]]
    -- 「リストは要素の数の語、くっつけると全部の組、Nothing は0」
    -- 「（Nothing は、くっつけても0）」
    it "makes words of lists, glued words and Nothing" $ do
      let source = T.unlines
            [ "none : Maybe Int"
            , "none = Nothing"
            , "main = do"
            , "  _ <- Shell.run"
            , "    $\"tar {[\"a\", \"b\"]} -x={[1, 2]} --skip={none} {none}\""
            , "  pure ()" ]
          (_, w) = script emptyWorld source
      map commandArgs (worldRan w) `shouldBe` [["a", "b", "-x=1", "-x=2"]]
    -- ANCHOR_END: shellWords

    -- ANCHOR: shellPipe
    -- 「パイプは、最初の出力を次の入力に。フォルダーと環境変数も届く」
    it "pipes the output on, in the folder, with the variables" $ do
      let world = emptyWorld { worldCommands = Map.fromList
            [ (["git", "log"], (0, "b\na\n", ""))
            , (["sort"], (0, "a\nb\n", "")) ] }
          source = T.unlines
            [ "main = do"
            , "  r <- Shell.lines ($\"git log\" .| $\"sort\""
            , "         |> Shell.within \"repo\""
            , "         |> Shell.withEnv {\"LC_ALL\": \"C\"})"
            , "  putStrLn (show r)" ]
          (_, w) = script world source
      outputOf w `shouldBe` "Ok [\"a\", \"b\"]\n"
      reverse (worldRan w) `shouldBe`
        [ ShellCommand "git" ["log"] "repo" [("LC_ALL", "C")] ""
        , ShellCommand "sort" [] "repo" [("LC_ALL", "C")] "b\na\n" ]
    -- 「最初が失敗したら、次は動かさない（pipefail と同じ）」
    it "stops a pipe at the first that fails" $ do
      let world = emptyWorld { worldCommands = Map.fromList
            [ (["git", "log"], (128, "", "fatal: no repo\n")) ] }
          source = T.unlines
            [ "main = do"
            , "  r <- Shell.out ($\"git log\" .| $\"sort\")"
            , "  putStrLn (show r)" ]
          (_, w) = script world source
      outputOf w `shouldBe`
        "Err \"git log | sort: exit 128: fatal: no repo\"\n"
      map commandName (worldRan w) `shouldBe` ["git"]
    -- ANCHOR_END: shellPipe

    -- ANCHOR: shellColors
    -- 「色は、端末で NO_COLOR がないときだけ」
    it "colors only a terminal without NO_COLOR" $ do
      let source = "main = Term.say (Term.red \"no\")"
          run w = outputOf (snd (script w source))
      run emptyWorld `shouldBe` "no\n"
      run emptyWorld { worldTerminal = True }
        `shouldBe` "\ESC[31mno\ESC[0m\n"
      run emptyWorld { worldTerminal = True
                     , worldEnv = Map.fromList [("NO_COLOR", "1")] }
        `shouldBe` "no\n"
      -- 「空の NO_COLOR は、ないのと同じ」
      run emptyWorld { worldTerminal = True
                     , worldEnv = Map.fromList [("NO_COLOR", "")] }
        `shouldBe` "\ESC[31mno\ESC[0m\n"
    -- 「印を外すと、元の文字」
    it "strips what the styles put in" $
      property $ forAll genText $ \t ->
        let source = "main = putStr (Term.strip (Term.bold (Term.red "
                       <> renderString t <> ")))"
        in outputOf (snd (script emptyWorld source)) === t
    -- ANCHOR_END: shellColors

    -- ANCHOR: shellTable
    -- 「表の行は、幅の広い文字があっても、同じ幅にそろう」
    it "lines up the columns, with wide letters too" $ do
      let source = T.unlines
            [ "main = Term.say (Term.table [\"branch\", \"author\"]"
            , "  [[\"機能/表\", \"Ruby\"], [\"fix-typo\", \"Python\"]])" ]
          shown = T.lines (outputOf (snd (script emptyWorld source)))
          columnOf l = displayWidth (fst (T.breakOnEnd "  " l))
      shown `shouldBe`
        [ "branch    author", "--------  ------"
        , "機能/表   Ruby", "fix-typo  Python" ]
      map columnOf shown `shouldBe` [10, 10, 10, 10]
    -- 「表に印は入らない。マスの改行は ⏎ で、行は1行のまま」
    it "puts no styles in a table, and keeps a row in one line" $ do
      let source = T.unlines
            [ "main = putStrLn (Term.table [\"api\", \"body\"]"
            , "  [[\"slow\", \"not found\\n\"], [\"tab\", \"a\\tb\"]])" ]
      T.lines (outputOf (snd (script emptyWorld source))) `shouldBe`
        [ "api   body", "----  ----------"
        , "slow  not found⏎", "tab   a b" ]
    -- 「漢字は2桁、印は0桁」
    it "counts the columns of a text" $
      map displayWidth ["abc", "機能", "\ESC[31mred\ESC[0m", "が"]
        `shouldBe` [3, 4, 3, 2]
    -- ANCHOR_END: shellTable

    -- ANCHOR: shellAsk
    -- 「y か yes だけが、はい。入力の終わりは、いいえ」
    it "says yes only for y and yes" $ do
      let source = T.unlines
            [ "main = do"
            , "  sure <- Term.confirm \"go?\""
            , "  putStrLn (show sure)" ]
          answer input = outputOf (snd (script emptyWorld
                           { worldInput = input } source))
      map answer [["y"], [" YES "], ["n"], [""], []]
        `shouldBe` ["true\n", "true\n", "false\n", "false\n", "false\n"]
    -- 「番号になるまで、もう一度聞く」
    it "asks again until the answer is a number of a choice" $ do
      let source = T.unlines
            [ "main = do"
            , "  c <- Term.select \"which?\" [\"a\", \"b\"]"
            , "  putStrLn (show c)" ]
          (_, w) = script emptyWorld { worldInput = ["x", "0", "2"] } source
      outputOf w `shouldBe` "Just \"b\"\n"
      errorsOf w `shouldBe` T.concat
        [ "1) a\n2) b\n", "which? (a number from 1 to 2)\n"
        , "which? (a number from 1 to 2)\n", "which? " ]
    -- 「端末でなければ、進み具合の棒は描かない」
    it "draws the bar only on a terminal" $ do
      let source = "main = Term.progress 1 2 \"a\""
          run w = outputOf (snd (script w source))
      run emptyWorld `shouldBe` ""
      run emptyWorld { worldTerminal = True }
        `shouldBe` "\r[##########          ] 1/2 a\ESC[K"
    -- ANCHOR_END: shellAsk

    -- ANCHOR: shellPrune
    -- 「題材のスクリプトを偽物の世界で：はいと答えると、消す」
    it "prunes the merged branches after a yes" $ do
      source <- Utf8.readFile "examples/shell/prune.lam"
      let world = emptyWorld
            { worldInput = ["y"]
            , worldCommands = Map.fromList
                [ (["git", "branch", "--show-current"], (0, "main\n", ""))
                , ( ["git", "branch", "--merged", "main"
                    , "--format=%(refname:short)"]
                  , (0, "fix-typo\nmain\n", "") )
                , ( ["git", "log", "-1", "--format=%an%x09%cs", "fix-typo"]
                  , (0, "Python\t2026-11-20\n", "") )
                , (["git", "branch", "-d", "fix-typo"], (0, "", "")) ] }
          (ending, w) = script world source
      ending `shouldBe` Finished ""
      map commandArgs (take 1 (worldRan w))
        `shouldBe` [["branch", "-d", "fix-typo"]]
      outputOf w `shouldBe` T.unlines
        [ "branch    author  date", "--------  ------  ----------"
        , "fix-typo  Python  2026-11-20", "deleted 1 of 1" ]
    -- 「--allow-run が git を許さなければ、git は動かない」
    it "runs no git without --allow-run=git" $ do
      dbs <- memoryDatabases
      (_, w) <- scriptFlagged dbs ["--allow-run=ls"] emptyWorld
        "main = do\n  r <- Shell.out $\"git status\"\n  putStrLn (show r)"
      outputOf w `shouldBe` "Err \"git: needs --allow-run=git\"\n"
      worldRan w `shouldBe` []
    -- 「--trace は、$"…" の形で1行に」
    it "traces a command as $\"...\" writes it" $
      traceLine (RunCommand (ShellCommand "git" ["branch", "-d", "old fix"]
                               "repo" [] ""))
        `shouldBe` "Shell.run $\"git branch -d 'old fix'\" (in repo)"
    -- ANCHOR_END: shellPrune

    -- ANCHOR: shellGolden
    -- 「本物の git の要らない例は、偽物の世界でも本物と同じ」
    mapM_ shellGolden ["words", "naive", "colors"]
    -- ANCHOR_END: shellGolden

  -- 「ワンライナー」
  describe "one-liners" $ do
    -- ANCHOR: oneLinerGoldenTest
    -- 「例のワンライナーは、偽物の世界でも本物の lam と同じ答えを出す
    --   （本物の git が要るものは除く）」
    it "gives the same answers as the real lam" $ do
      names <- listDirectory "examples/oneliner"
      let cmds = [ dropExtension n | n <- names, takeExtension n == ".cmd"
                 , (dropExtension n <> ".git") `notElem` names ]
      length cmds `shouldSatisfy` (>= 35)
      mapM_ (\n -> do
               let base = "examples/oneliner" </> n
               want <- Utf8.readFile (base <> ".out")
               got  <- runOneLinerExample base
               (n, got) `shouldBe` (n, want))
            cmds
    -- ANCHOR_END: oneLinerGoldenTest

    -- ANCHOR: readCommandTest
    -- 「短いフラグは、Perl と同じくまとめて書ける」
    it "reads short flags together, as Perl does" $ do
      let one args = fmap snd (readCommand args Nothing)
      one ["-ane", "x", "a.txt"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = EachLine
                   , oneSplit = ByWords } ["a.txt"])
      one ["-F,", "-pe", "x"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = Printing
                   , oneSplit = ByRegex "," } [])
      one ["-i.bak", "-pex", "f"] `shouldBe` Right (RunOneLiner
        noOneLiner { oneProgram = ["x"], oneMode = Printing
                   , oneInPlace = Just ".bak" } ["f"])
      one ["-MPrice", "-M", "Shop as S", "-e", "1"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["1"]
                                , oneModules = ["Price", "Shop as S"] } [])
      one ["-n", "--begin", "0", "-e", "acc", "--end", "print", "a"]
        `shouldBe` Right (RunOneLiner
          noOneLiner { oneProgram = ["acc"], oneMode = EachLine
                     , oneBegin = Just "0", oneEnd = Just "print" } ["a"])
      one ["--trace", "prog.lam", "-n"]
        `shouldBe` Right (RunFile "prog.lam" ["-n"])
    -- 「プログラムの後ろの語は、ファイルと同じく、プログラムのもの」
    it "gives the words after the program to the program" $ do
      let one args = fmap snd (readCommand args Nothing)
      one ["-e", "main", "--name", "Lam", "-n"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["main"] }
           ["--name", "Lam", "-n"])
      one ["--decoding=strict", "-ne", "x", "--", "-"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["x"], oneMode = EachLine
                                , oneDecoding = Just Strict } ["-"])
    -- 「--print-script は、プログラムの後ろにも書ける。ほかの lam の
    -- フラグは、-e より前に、と言う」
    it "takes --print-script after the program too, and no other flag" $ do
      let one args = fmap snd (readCommand args Nothing)
      one ["-ne", "x", "--print-script=plain", "a.log"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["x"], oneMode = EachLine
                                , oneScript = Just PlainForm } ["a.log"])
      one ["-ne", "x", "--", "--print-script"] `shouldBe` Right
        (RunOneLiner noOneLiner { oneProgram = ["x"], oneMode = EachLine }
           ["--print-script"])
      one ["-ne", "x", "--dry-run", "a.log"] `shouldBe`
        Left "lam: --dry-run goes before -e (after the program come the \
             \files)"
    -- 「一緒に使えないフラグや、知らないフラグは、動かす前に止める」
    it "stops flags that do not go together, before running" $
      map (either Left (const (Right ())) . flip readCommand Nothing)
        [ ["-n", "a.txt"], ["-i", "-ne", "x", "f"], ["-i", "-pe", "x"]
        , ["--begin", "0", "-pe", "x"], ["--end", "x", "-ne", "x"]
        , ["-F(", "-e", "x"], ["-x"], ["-e"], ["--as=Order", "-ne", "x"]
        , ["--decoding=latin1", "-ne", "x"] ]
        `shouldBe`
          map Left
            [ "lam: -n, -p, -a, -F, -i, -M, --begin and --end need -e 'program'"
            , "lam: -i needs -p (each line becomes what the program gives)"
            , "lam: -i needs the files to change"
            , "lam: --begin and --end go with -n, not with -p"
            , "lam: --end needs --begin (the first acc)"
            , "lam: -F: missing closing parenthesis"
            , "lam: unknown flag: -x"
            , "lam: -e needs something after it"
            , "lam: --as goes with --json or --jsonl"
            , "lam: --decoding=strict, --decoding=replace or \
              \--decoding=ignore" ]
    -- ANCHOR_END: readCommandTest

    -- ANCHOR: byTypeTest
    -- 「s/a/b/g と s/a/b/g line は、どの行にも同じ答えを出す」
    it "gives the same lines for s/a/b/g and s/a/b/g line" $
      property $ forAll (listOf genLine) $ \ls -> ioProperty $ do
        a <- printed "s/a+/<{0}>/g" ls
        b <- printed "s/a+/<{0}>/g line" ls
        pure (a === b)
    -- 「-p の答えは、Haskell で1行ずつ変えたものと同じ」
    it "prints each line changed, as Haskell would" $
      property $ forAll (listOf genLine) $ \ls -> ioProperty $ do
        got <- printed "toUpper" ls
        pure (got === T.concat [ T.toUpper l <> "\n" | l <- ls ])
    -- ANCHOR_END: byTypeTest

    -- ANCHOR: remapTest
    -- 「スクリプトの場所のエラーは、人が書いた場所で言う」
    it "tells an error at the place in -e" $ do
      let one = noOneLiner { oneProgram = ["a", "  b"], oneMode = EachLine }
          script = Script "" [Part "-e" 11 2 2]
      remap one script "<one-liner>:12:6: in the definition of step: x"
        `shouldBe` "-e:2:4: x"
      remap one script "<one-liner>:30:1: y" `shouldBe` "-e: y"
      remap one script "line 1\nline 2" `shouldBe` "line 1\nline 2"
    -- ANCHOR_END: remapTest

    -- ANCHOR: jsonTest
    -- 「配列を --json で読んでも、1行ずつ --jsonl で読んでも、同じ答え」
    it "gives the same for an array with --json and for --jsonl" $
      property $ forAll (listOf (chooseInteger (-50, 50))) $ \ns ->
        ioProperty $ do
          let objects = [ "{\"n\": " <> T.pack (show n) <> "}" | n <- ns ]
              array   = "[" <> T.intercalate ", " objects <> "]"
              program = ["Json.get \"n\" json ?: JNull"]
          a <- printedFrom array
                 noOneLiner { oneProgram = program, oneMode = Printing
                            , oneInput = WholeJson }
          b <- printedFrom (T.unlines objects)
                 noOneLiner { oneProgram = program, oneMode = Printing
                            , oneInput = JsonLines }
          pure (a === b)
    -- ANCHOR_END: jsonTest

    -- ANCHOR: scriptTest
    -- 「--print-script が書いたスクリプトを、ファイルとして動かしても、
    --   ワンライナーと同じ答え」
    it "writes scripts that do what the one-liners do" $ do
      names <- listDirectory "examples/oneliner"
      let cmds = [ dropExtension n | n <- names, takeExtension n == ".cmd"
                 , (dropExtension n <> ".git") `notElem` names
                 , dropExtension n /= "unknown" ]
      checked <- fmap concat $ mapM (\n -> do
        let base = "examples/oneliner" </> n
        (opts, one, rest, world) <- exampleWorld base
        files <- exampleFiles
        dbs <- memoryDatabases
        guard <- guarded opts
        (made, _) <- runFakeIO world
          (prepare files preludeSession preludeSession one)
        case (made, oneMode one) of
          (Right script, mode) | mode /= Once, oneScript one == Nothing -> do
            (e1, w1) <- runFakeIO world { worldArgs = map T.pack rest }
              (runOneLiner (guard (fakeWorldIO dbs)) files preludeSession one)
            dbs2 <- memoryDatabases
            guard2 <- guarded opts
            (e2, w2) <- runFakeIO world { worldArgs = map T.pack rest } $ do
              loaded <- loadProgram files preludeSession preludeSession
                          "<one-liner>" (scriptText script)
              case loaded of
                Left err -> pure (Errored err)
                Right (done, final) ->
                  runProgram (guard2 (fakeWorldIO dbs2)) done final
            -- The script names its own places; told at the places of
            -- -e, they are the same.
            -- スクリプトは自分の場所を言う。-e の場所で言えば、同じ
            let placed e = case e of
                  Errored err -> Errored (remap one script err)
                  other       -> other
            (n, shownRun world (placed e2) w2)
              `shouldBe` (n, shownRun world e1 w1)
            pure [n]
          _ -> pure [])
        cmds
      length checked `shouldSatisfy` (>= 25)
    -- ANCHOR_END: scriptTest

    -- ANCHOR: inPlaceTest
    -- 「-i.bak は、元の中身を残してから書き換える。途中で止まったら、
    --   ファイルは元のまま。--dry-run なら何も書かない」
    it "keeps a backup, keeps the file when it stops, and dry-runs" $ do
      let world = emptyWorld
            { worldFiles = Map.fromList [("a.txt", "red\n\nblue\n")] }
          run args = do
            (opts, command) <- either (fail . T.unpack) pure
                                 (readCommand args Nothing)
            (one, rest) <- case command of
              RunOneLiner one rest -> pure (one, rest)
              other -> fail (show other)
            dbs <- memoryDatabases
            guard <- guarded opts
            runFakeIO world { worldArgs = map T.pack rest }
              (runOneLiner (guard (fakeWorldIO dbs)) noFiles preludeSession
                 one)
      (ending, after) <- run ["-i.bak", "-pe", "toUpper", "a.txt"]
      ending `shouldBe` Finished ""
      worldFiles after `shouldBe` Map.fromList
        [("a.txt", "RED\n\nBLUE\n"), ("a.txt.bak", "red\n\nblue\n")]
      (stopped, kept) <- run
        ["-i", "-pe", "if line == \"\" then error \"empty\" else line", "a.txt"]
      stopped `shouldBe` Errored "-e:1:20: error: empty"
      worldFiles kept `shouldBe` worldFiles world
      (_, dry) <- run ["--dry-run", "-i", "-pe", "toUpper", "a.txt"]
      worldFiles dry `shouldBe` worldFiles world
    -- ANCHOR_END: inPlaceTest

    -- ANCHOR: inPlaceStrictTest
    -- 「-i は、--decoding がなければ strict。壊れたバイトのあるファイルが
    --   1つでもあれば、どのファイルも書き換えない。ignore と書けば通る」
    it "changes no file with -i when one has a broken byte" $ do
      let files = Map.fromList
            [("a.txt", "red\n"), ("b.txt", "blue \xff sky\n")]
          world = emptyWorld { worldFiles = files }
          run args = do
            (opts, command) <- either (fail . T.unpack) pure
                                 (readCommand args Nothing)
            (one, rest) <- case command of
              RunOneLiner one rest -> pure (one, rest)
              other -> fail (show other)
            dbs <- memoryDatabases
            guard <- guarded opts
            runFakeIO world { worldArgs = map T.pack rest }
              (runOneLiner (guard (fakeWorldIO dbs)) noFiles preludeSession
                 one)
      (stopped, kept) <- run ["-i", "-pe", "toUpper", "a.txt", "b.txt"]
      stopped `shouldBe`
        Errored "error: b.txt: line 1: not UTF-8 from byte 5"
      worldFiles kept `shouldBe` files
      (ending, after) <-
        run ["--decoding=ignore", "-i", "-pe", "toUpper", "a.txt", "b.txt"]
      ending `shouldBe` Finished ""
      worldFiles after `shouldBe` Map.fromList
        [("a.txt", "RED\n"), ("b.txt", "BLUE  SKY\n")]
    -- ANCHOR_END: inPlaceStrictTest

    -- ANCHOR: forLinesTest
    -- 「File.forLines は、"-" なら標準入力を、番号つきで1行ずつ読む」
    it "reads the standard input as \"-\", with the numbers" $ do
      let source = T.unlines
            [ "main : IO (Result String ())"
            , "main = do"
            , "  n <- File.forLines Ignore \"-\" 0 (\\acc n line -> do"
            , "    putStrLn f\"{n} {line}\""
            , "    pure (acc + 1)) ?"
            , "  print n"
            , "  pure (Ok ())" ]
          (_, w) = script emptyWorld { worldInput = ["a", "b"] } source
      outputOf w `shouldBe` "1 a\n2 b\n2\n"
    -- 「Decode.Decodable は、型が Decoder を選ぶ」
    it "reads JSON by its type with Decode.Decodable" $ do
      let source = T.unlines
            [ "xs : Result [Problem] [Maybe Int]"
            , "xs = Decode.fromString Decode.decoder \"[1, null]\""
            , "main : IO ()"
            , "main = print xs" ]
          (_, w) = script emptyWorld source
      outputOf w `shouldBe` "Ok [Just 1, Nothing]\n"
    -- ANCHOR_END: forLinesTest

  -- 「並行に動かす」
  describe "at once" $ do
    -- ANCHOR: concurrencyGolden
    -- On the fake clock: the same answers and times every run.
    -- 偽の時計の上で：答えも時間も、いつも同じ
    mapM_ (golden . ("concurrency/" <>))
      [ "one-by-one", "all", "jobs", "failures", "race", "sleepy"
      , "partial" ]
    -- ANCHOR_END: concurrencyGolden

    -- ANCHOR: traceAtOnce
    -- 「--trace の時間で、同時に始まったものがわかる」
    it "shows in --trace which requests started at once" $ do
      dbs <- memoryDatabases
      let traceOf base = do
            (_, w) <- runFlagged dbs id ["--trace"]
                        ("examples/concurrency/" <> base <> ".lam")
            pure [ T.takeWhile (/= ':') l | l <- T.lines (errorsOf w)
                                          , "Http" `T.isInfixOf` l ]
      traceOf "one-by-one" `shouldReturn`
        [ "trace 0.000s", "trace 0.300s", "trace 0.800s", "trace 1.000s"
        , "trace 1.400s", "trace 1.700s" ]
      traceOf "all" `shouldReturn` replicate 6 "trace 0.000s"
    -- ANCHOR_END: traceAtOnce

    -- ANCHOR: fakeClockTests
    -- 「偽の時計：同時の仕事は、いちばん遅い終わりまで進む」
    it "moves the fake clock to the latest end" $ do
      let clockAfter io = worldClock (snd (script emptyWorld
                            ("main : IO ()\nmain = do\n  _ <- " <> io
                             <> "\n  pure ()\n"))) - fakeClock
      clockAfter "IO.all [sleep 300, sleep 100]" `shouldBe` 300
      clockAfter "IO.allN 1 [sleep 300, sleep 100]" `shouldBe` 400
      -- 「race は、いちばん早い終わりで止まる」
      clockAfter "IO.race [sleep 300, sleep 100]" `shouldBe` 100
    -- 「答えは、どんな待ち時間でもリストの順。時計はいちばん長い待ちだけ」
    it "keeps the answers in the order of the list" $
      property $ \(ms :: [Int]) ->
        let delays = map (\m -> toInteger (m `mod` 1000)) ms
            src = "main : IO ()\nmain = do\n"
                  <> "  xs <- IO.all (map (\\d -> map (\\_ -> d) (sleep d)) "
                  <> lamInts delays <> ")\n"
                  <> "  print xs\n"
            (_, w) = script emptyWorld src
        in outputOf w === lamInts delays <> "\n"
           .&&. worldClock w - fakeClock === maximum (0 : delays)
    -- 「allOk：最初の Err のあとに始まる仕事は、始まらない」
    it "does not start a job after the first Err of IO.allOk" $ do
      let jobs = [ "job : String -> Int -> Bool -> IO (Result String String)"
                 , "job name ms ok = do"
                 , "  sleep ms"
                 , "  putStrLn name"
                 , "  pure (if ok then Ok name else Err name)" ]
          (_, w) = script emptyWorld $ T.unlines $ jobs <>
            [ "main : IO ()"
            , "main = do"
            , "  r <- IO.allN 1 [job \"a\" 100 true, job \"b\" 100 false,"
            , "                  job \"c\" 100 true]"
            , "  print r" ]
      outputOf w `shouldBe` "a\nb\nc\n[Ok \"a\", Err \"b\", Ok \"c\"]\n"
      let (_, w2) = script emptyWorld $ T.unlines $ jobs <>
            [ "main : IO ()"
            , "main = do"
            , "  r <- IO.allOk [job \"a\" 300 true, job \"b\" 100 false]"
            , "  print r"
            , "  r2 <- IO.allN 1 [ IO.allOk [job \"c\" 100 false]"
            , "                  , IO.allOk [job \"d\" 1 true] ]"
            , "  print r2" ]
      -- b fails at 100, and a (still asleep until 300) is cancelled,
      -- as in the real world: the jobs take turns now (Lam.Scheduler).
      -- c fails, but d is a job of IO.allN, not of that IO.allOk: it
      -- runs.
      -- b は 100 で失敗し、（300 まで寝ている）a は取り消される。本物の
      -- 世界と同じ：仕事は順番に動くようになった（Lam.Scheduler）。c も
      -- 失敗するが、d は IO.allN の仕事で、その IO.allOk の仕事ではない
      outputOf w2 `shouldBe`
        "b\nErr \"b\"\nc\nd\n[Err \"c\", Ok [\"d\"]]\n"
    -- 「spawn：すぐに先へ進み、await で終わりまで待つ」
    it "goes on at once after spawn, and waits at await" $ do
      let (_, w) = script emptyWorld $ T.unlines
            [ "main : IO ()"
            , "main = do"
            , "  t <- spawn (map (\\_ -> 7) (sleep 500))"
            , "  a <- Time.now"
            , "  n <- await t"
            , "  b <- Time.now"
            , "  print (n, Time.diffSeconds b a)" ]
      outputOf w `shouldBe` "(7, 0)\n"
      worldClock w - fakeClock `shouldBe` 500
    -- 「spawn・both・Par の例：ファイルは偽物の世界に置く」
    it "runs the examples that read party.txt" $ do
      dbs <- memoryDatabases
      party <- B.readFile "examples/concurrency/party.txt"
      let withParty w = w { worldFiles = Map.insert "party.txt" party
                                           (worldFiles w) }
          run base = fst <$> runExampleWith dbs withParty
                               ("examples/concurrency/" <> base <> ".lam")
      run "spawn" `shouldReturn` T.unlines
        [ "party: Pikachu, Eevee, Snorlax"
        , "#25   ピカチュウ (pikachu)", "#133  イーブイ (eevee)"
        , "#143  カビゴン (snorlax)" ]
      run "both" `shouldReturn`
        "#25   ピカチュウ (pikachu), in a party of 3\n"
      want <- Utf8.readFile "examples/concurrency/together.out"
      run "together" `shouldReturn` want
    -- ANCHOR_END: fakeClockTests

    -- ANCHOR: checkAllFake
    -- 「コマンドも同時に：見張りは、どの仕事の頼みごとも通す」
    it "runs the checks a few at once, through the guards" $ do
      dbs <- memoryDatabases
      names <- listDirectory "examples/concurrency/checks"
      files <- mapM (\n -> (,) ("checks/" <> T.pack n) <$>
                       B.readFile ("examples/concurrency/checks/" <> n))
                    names
      -- divide.lam fails with exit 1; the others say ok.
      -- divide.lam は exit 1 で通らない。ほかは ok と言う
      let result n = if n == "divide.lam" then (1, "", "") else (0, "ok\n", "")
          setUp w = w
            { worldFiles = Map.fromList files
            , worldCommands = Map.fromList
                [ (["lam", "checks/" <> T.pack n], result n) | n <- names ] }
          run flags = runFlagged dbs setUp flags
                        "examples/concurrency/check-all.lam"
      want <- Utf8.readFile "examples/concurrency/check-all.out"
      (shown, w) <- run ["--allow-read", "--allow-run=lam"]
      shown `shouldBe` want
      length (worldRan w) `shouldBe` 6
      -- Without --allow-run=lam, not one of the six jobs starts lam.
      -- --allow-run=lam がなければ、6つの仕事のどれも lam を動かさない
      (denied, w2) <- run ["--allow-read"]
      worldRan w2 `shouldBe` []
      T.lines denied `shouldSatisfy`
        (elem "6 checks, 6 failed")
    -- ANCHOR_END: checkAllFake

    -- ANCHOR: oneLinerAtOnce
    -- 「ワンライナーの --end でも、まとめて待てる」
    it "fetches the lines of a one-liner at once" $ do
      cmd  <- Utf8.readFile "examples/concurrency/oneliner.cmd"
      want <- Utf8.readFile "examples/concurrency/oneliner.out"
      names <- B.readFile "examples/concurrency/names.txt"
      webLines <- T.lines <$> Utf8.readFile "examples/concurrency/oneliner.web"
      web <- mapM (\l -> case T.words l of
                      [u, st, f, ms] -> do
                        body <- Utf8.readFile ("examples/concurrency/" <> T.unpack f)
                        pure (u, (read (T.unpack st), body), read (T.unpack ms))
                      _ -> fail "a bad line") webLines
      modules <- lamFiles "examples/concurrency"
      let files = Files (\f -> pure (Map.lookup f (Map.fromList modules)))
                        ["examples/concurrency"]
      one <- case readCommand (oneLinerWords cmd) Nothing of
        Right (_, RunOneLiner o rest) -> pure (o, rest)
        _ -> fail "not a one-liner"
      dbs <- memoryDatabases
      (_, after) <- runFakeIO emptyWorld
        { worldFiles = Map.fromList [("names.txt", names)]
        , worldArgs = map T.pack (snd one)
        , worldWeb = Map.fromList [ (u, r) | (u, r, _) <- web ]
        , worldDelays = Map.fromList [ (u, d) | (u, _, d) <- web ] }
        (runOneLiner (fakeWorldIO dbs) files preludeSession (fst one))
      outputOf after `shouldBe` want
      -- pikachu, eevee and snorlax at once: the longest is 0.6 s.
      -- ピカチュウ・イーブイ・カビゴンを同時に：いちばん長いのは 0.6 秒
      worldClock after - fakeClock `shouldBe` 600
    -- ANCHOR_END: oneLinerAtOnce

    -- ANCHOR: realAtOnce
    -- 「本物の世界：本当に同時に動き、取り消す」
    it "really waits at once, and cancels the others" $ do
      -- five naps of 0.2 s at once: not 1 s
      -- 0.2 秒の昼寝5回を同時に：1 秒ではない
      (e1, t1) <- realRun $ T.unlines
        [ "main : IO ()"
        , "main = do"
        , "  _ <- IO.all (map (\\_ -> sleep 200) [1, 2, 3, 4, 5])"
        , "  exit 3" ]
      e1 `shouldBe` ExitedWith 3
      t1 `shouldSatisfy` (< 0.8)
      -- the Err at 0.1 s stops the job of 5 s
      -- 0.1 秒の Err が、5 秒の仕事を取り消す
      (e2, t2) <- realRun $ T.unlines
        [ "main : IO ()"
        , "main = do"
        , "  r <- IO.allOk [ map (\\_ -> Ok 1) (sleep 5000)"
        , "                , map (\\_ -> Err 2) (sleep 100) ]"
        , "  case r of"
        , "    | Err n -> exit n"
        , "    | Ok _ -> exit 9" ]
      e2 `shouldBe` ExitedWith 2
      t2 `shouldSatisfy` (< 2)
      -- exit in one job stops the program, and the job of 5 s
      -- 1つの仕事の exit で、プログラムも 5 秒の仕事も止まる
      (e3, t3) <- realRun $ T.unlines
        [ "main : IO ()"
        , "main = do"
        , "  _ <- IO.all [ sleep 5000, bind (sleep 100) (\\_ -> exit 4) ]"
        , "  exit 9" ]
      e3 `shouldBe` ExitedWith 4
      t3 `shouldSatisfy` (< 2)
      -- the race and the time limit
      -- 競走と制限時間
      (e4, t4) <- realRun $ T.unlines
        [ "main : IO ()"
        , "main = do"
        , "  r <- IO.timeout 100 (map (\\_ -> 1) (sleep 5000))"
        , "  exit (fromMaybe 6 r)" ]
      e4 `shouldBe` ExitedWith 6
      t4 `shouldSatisfy` (< 2)
      -- ado on Par: two naps of 0.3 s at once
      -- Par の ado：0.3 秒の昼寝2つを同時に
      par <- Utf8.readFile "examples/concurrency/Par.lam"
      (e5, t5) <- realRunWith [("Par.lam", par)] $ T.unlines
        [ "import Par (Par (..))"
        , "main : IO ()"
        , "main = do"
        , "  r <- Par.run (ado"
        , "    a <- Par (map (\\_ -> 1) (sleep 300))"
        , "    b <- Par (map (\\_ -> 2) (sleep 300))"
        , "    pure (a + b))"
        , "  exit r" ]
      e5 `shouldBe` ExitedWith 3
      t5 `shouldSatisfy` (< 0.55)
    -- ANCHOR_END: realAtOnce

    -- ANCHOR: realCleanup
    -- 「取り消された仕事も、defer の後始末をする」
    it "cleans up a job that was cancelled" $ do
      dir <- getTemporaryDirectory
      let part = dir </> "lam-concurrency-cleanup.part"
      (e, t) <- realRun $ T.unlines
        [ "slow : IO (Result String ())"
        , "slow = do"
        , "  writeFile " <> renderString (T.pack part) <> " \"half\" ?"
        , "  defer File.remove " <> renderString (T.pack part)
        , "  sleep 5000"
        , "  pure (Ok ())"
        , "main : IO ()"
        , "main = do"
        , "  r <- IO.timeout 200 slow"
        , "  exit (if r == Nothing then 5 else 0)" ]
      e `shouldBe` ExitedWith 5
      t `shouldSatisfy` (< 2)
      doesFileExist part `shouldReturn` False
    -- ANCHOR_END: realCleanup

  -- 「仕事どうしで話す」
  describe "talking jobs" $ do
    -- ANCHOR: channelsGolden
    -- On the fake clock: the same turns, answers, and times every run.
    -- 偽の時計の上で：番の順も、答えも、時間も、いつも同じ
    mapM_ golden
      [ "channels/progress", "channels/buffer", "channels/pool"
      , "channels/counter", "channels/selective", "channels/crash"
      , "channels/monitor", "channels/supervise", "channels/deadlock"
      , "channels/first", "channels/broken/wrong-message" ]
    -- ANCHOR_END: channelsGolden

    -- ANCHOR: checkLiveFake
    -- 「確かめを、終わったものから。偽物の世界のコマンドは、すぐ終わる」
    it "shows the checks as they end" $ do
      dbs <- memoryDatabases
      names <- listDirectory "examples/channels/checks"
      files <- mapM (\n -> (,) ("checks/" <> T.pack n) <$>
                       B.readFile ("examples/channels/checks/" <> n))
                    names
      let result n = if n == "divide.lam" then (1, "", "") else (0, "ok\n", "")
          setUp w = w
            { worldFiles = Map.fromList files
            , worldCommands = Map.fromList
                [ (["lam", "checks/" <> T.pack n], result n) | n <- names ] }
      want <- Utf8.readFile "examples/channels/check-live.out"
      (shown, w) <- runFlagged dbs setUp ["--allow-read", "--allow-run=lam"]
                      "examples/channels/check-live.lam"
      shown `shouldBe` want
      length (worldRan w) `shouldBe` 7
    -- ANCHOR_END: checkLiveFake

    -- ANCHOR: chanProperties
    -- 「チャネルは、送った順に届ける。容量がいくつでも」
    it "delivers in the order sent, whatever the capacity" $
      property $ \(xs :: [Integer]) (NonNegative k) ->
        let src = T.unlines
              [ "main = do"
              , "  c <- Chan.new " <> T.pack (show (k `mod` 4 :: Int))
              , "  _ <- spawn (do"
              , "    forEach " <> lamInts xs <> " (\\x -> c ! x)"
              , "    Chan.close c)"
              , "  ys <- Chan.toList c"
              , "  putStrLn (display ys)" ]
        in outputOf (snd (script emptyWorld src)) === lamInts xs <> "\n"
    -- 「受け箱：合わないメッセージは、順のまま残る」
    it "keeps the messages that do not fit, in order" $
      property $ \(Positive n) ->
        let m = n `mod` 8 + 1 :: Integer
            src = T.unlines
              [ "take me k = receive me"
              , "  | x if x == k -> pure x"
              , "main = do"
              , "  me <- Actor.new"
              , "  forEach (range 1 " <> T.pack (show m)
                  <> ") (\\x -> me ! x)"
              , "  ys <- mapM (take me) (reverse (range 1 "
                  <> T.pack (show m) <> "))"
              , "  putStrLn (display ys)" ]
        in outputOf (snd (script emptyWorld src))
             === lamInts (reverse [1 .. m]) <> "\n"
    -- ANCHOR_END: chanProperties

    -- ANCHOR: sendable
    -- 「! は、チャネルにも受け箱にも送る（クラス Sendable）」
    it "sends with ! to a channel and to a mailbox" $ do
      let (_, w) = script emptyWorld $ T.unlines
            [ "main = do"
            , "  c <- Chan.new 1"
            , "  me <- Actor.new"
            , "  c ! \"to the channel\""
            , "  me ! \"to the mailbox\""
            , "  x <- Chan.recv c"
            , "  y <- receive me | m -> pure m"
            , "  print (x, y)" ]
      outputOf w `shouldBe` "(Just \"to the channel\", \"to the mailbox\")\n"
    -- 「! の型がちがえば、型検査器が止める」
    it "stops a message of another type" $ do
      let (ending, _) = script emptyWorld $ T.unlines
            [ "main = do"
            , "  c <- Chan.new 1"
            , "  c ! 1"
            , "  c ! \"two\"" ]
      ending `shouldSatisfy` \e -> case e of
        Errored t -> "type error" `T.isInfixOf` t
        _         -> False
    -- ANCHOR_END: sendable

    -- ANCHOR: fakeCleanup
    -- 「偽物の世界でも、取り消された仕事の後始末が動く」
    it "does the cleanups of a cancelled job in a fake world" $ do
      let (_, w) = script emptyWorld $ T.unlines
            [ "slow = do"
            , "  defer putStrLn \"slow: cleaned up\""
            , "  sleep 500"
            , "  putStrLn \"slow: done\""
            , "  pure \"slow\""
            , "quick = do"
            , "  sleep 100"
            , "  pure \"quick\""
            , "main = do"
            , "  start <- Time.now"
            , "  r <- IO.race [slow, quick]"
            , "  now <- Time.now"
            , "  putStrLn f\"{r} at {Time.toMillis now - Time.toMillis start}\"" ]
      outputOf w `shouldBe` "slow: cleaned up\nquick at 100\n"
    -- 「後始末が待てば、親もそれを待つ（本物の世界と同じ）」
    it "lets the parent wait for the cleanups" $ do
      let (_, w) = script emptyWorld $ T.unlines
            [ "slow = IO.finally (sleep 500) (do"
            , "  sleep 50"
            , "  putStrLn \"cleaned up\")"
            , "main = do"
            , "  start <- Time.now"
            , "  _ <- IO.race [slow, sleep 100]"
            , "  now <- Time.now"
            , "  print (Time.toMillis now - Time.toMillis start)" ]
      outputOf w `shouldBe` "cleaned up\n150\n"
    -- ANCHOR_END: fakeCleanup

    -- ANCHOR: seeds
    -- 「種を渡すと、同じ時刻の仕事の順が混ざる。同じ種なら同じ順」
    it "mixes the jobs of the same time with a seed" $ do
      source <- Utf8.readFile "examples/channels/first.lam"
      let winner seed = outputOf $ snd $ script
            emptyWorld { worldShuffle = Just (mkStdGen seed) } source
      -- Without a seed: always left (the order the jobs started).
      -- 種がなければ、いつも left（仕事を始めた順）
      outputOf (snd (script emptyWorld source)) `shouldBe` "first: left\n"
      -- With seeds: sometimes right. The same seed, the same answer.
      -- 種があれば、right のことも。同じ種なら、同じ答え
      nub (map winner [618 .. 657]) `shouldMatchList`
        ["first: left\n", "first: right\n"]
      map winner [618 .. 657] `shouldBe` map winner [618 .. 657]
    -- ANCHOR_END: seeds

    -- ANCHOR: fakeDeadlock
    -- 「偽物の世界：だれも進めなければ、だれが何を待っているかを言う」
    it "tells who waits for what in a deadlock" $ do
      let (ending, _) = script emptyWorld $ T.unlines
            [ "main = do"
            , "  c <- Chan.new 0"
            , "  d <- Chan.new 0"
            , "  t <- spawn (do"
            , "    _ <- Chan.recv c"
            , "    d ! 1)"
            , "  _ <- Chan.recv d"
            , "  await t" ]
      ending `shouldBe` Errored (T.intercalate "\n"
        [ "error: deadlock: every job is waiting, and none can go on"
        , "  main    waits to receive from <chan #2>"
        , "  job #1  waits to receive from <chan #1>" ])
    -- ANCHOR_END: fakeDeadlock

    -- ANCHOR: realChannels
    -- 「本物の世界：チャネルと受け箱が、スレッドのあいだで値を運ぶ」
    it "carries values between real threads" $ do
      -- 1 to 100 through a channel of 3, summed by 4 workers
      -- 1〜100 を容量3のチャネルで流し、4人の働き手で足す
      (e1, _) <- realRun $ T.unlines
        [ "data Msg = Add Int | Get (Pid Int)"
        , "counter total me = receive me"
        , "  | Add n -> counter (total + n) me"
        , "  | Get from -> do"
        , "      from ! total"
        , "      counter total me"
        , "work c sum = Chan.forEach c (\\n -> sum ! Add n)"
        , "main = do"
        , "  sum <- Actor.spawn (counter 0)"
        , "  c <- Chan.new 3"
        , "  _ <- spawn (do"
        , "    forEach (range 1 100) (\\n -> c ! n)"
        , "    Chan.close c)"
        , "  _ <- IO.all (map (\\_ -> work c sum) (range 1 4))"
        , "  total <- Actor.call sum Get"
        , "  exit (total // 100)" ]
      e1 `shouldBe` ExitedWith 50
      -- receive ... after: the timer wins when nobody sends
      -- receive … after：だれも送らなければ、タイマーが勝つ
      (e2, _) <- realRun $ T.unlines
        [ "main = do"
        , "  me <- Actor.new"
        , "  receive me"
        , "    | n -> exit n"
        , "    after 100 -> exit 7" ]
      e2 `shouldBe` ExitedWith 7
    -- ANCHOR_END: realChannels

  -- 「小さな Web サーバー」
  describe "a small web server" $ do
    -- ANCHOR: serverGolden
    -- 「偽物の世界の頼みに、偽の時計の上で答える」
    mapM_ golden
      [ "server/hello", "server/paths", "server/todo", "server/slow"
      , "server/oops", "server/static", "server/denied" ]
    -- ANCHOR_END: serverGolden
    -- ANCHOR: routeTests
    -- 「:id は、どのかけらにも合う」
    it "matches :id with any piece" $
      runLines [ "Route.matchPieces [\"todos\", \":id\"] [\"todos\", \"3\"]"
               , "Route.matchPieces [\"todos\"] [\"todos\", \"3\"]" ]
        `shouldBe` [ "Just {\"id\": \"3\"} : Maybe (Map String String)"
                   , "Nothing : Maybe (Map String String)" ]
    -- ANCHOR_END: routeTests
    -- 「ホストは、そのどのポートにも及ぶ」
    it "lets a host cover its ports" $ do
      covers Net (Within ["localhost"]) "localhost:8080" `shouldBe` True
      covers Net (Within ["localhost:9000"]) "localhost:8080" `shouldBe` False
      covers Net (Within ["pokeapi.co"]) "pokeapi.co" `shouldBe` True
    -- ANCHOR: bundleTests
    -- 「包みは、書いて読むと元どおり」
    it "reads back the bundle it wrote" $ do
      let b = Bundle "todo.lam" ["--allow-net=localhost"]
                [("todo.lam", "main = pure ()\n"), ("Todos.lam", "-- 📝\n")] []
          bytes = "lam itself" <> encodeBundle b
          (body, tailBytes) = B.splitAt (B.length bytes - trailerSize) bytes
          size = B.length (encodeBundle b) - trailerSize
      fmap fst (decodeBundle tailBytes (B.drop (B.length body - size) body))
        `shouldBe` Just b
    -- 「印のないファイルには、包みはない」
    it "finds no bundle in lam alone" $
      decodeBundle (B.replicate trailerSize 0) "" `shouldBe` Nothing
    -- ANCHOR_END: bundleTests

  -- 「言葉を補強する」
  describe "a sturdier language" $ do
    -- ANCHOR: hardeningGolden
    -- 「章の例を、偽物の世界で」
    mapM_ golden
      [ "hardening/memo", "hardening/parity", "hardening/table"
      , "hardening/nested", "hardening/keys", "hardening/broken/forgot"
      , "hardening/broken/plain-main", "hardening/broken/number"
      , "hardening/broken/outer", "hardening/broken/ring"
      , "hardening/cells", "hardening/broken/liar", "hardening/dropped" ]
    -- ANCHOR_END: hardeningGolden
    -- ANCHOR: droppedTest
    -- 「Result を捨てた行だけに警告。_ <- と 🗑️ <-、Maybe、最後の行は
    --   言わない。ラムダの中の do でも言う」
    it "warns about a Result thrown away, and only about that" $ do
      let warned source = fmap (sessionWarnings . fst)
            (loadPlain preludeSession "d.lam" source)
      warned (T.unlines
        [ "main = do"
        , "  writeFile \"a\" \"x\""
        , "  _ <- writeFile \"b\" \"x\""
        , "  🗑️ <- writeFile \"c\" \"x\""
        , "  🗑 <- writeFile \"d\" \"x\""
        , "  lookupEnv \"HOME\""
        , "  forEach [1, 2] (\\n -> do"
        , "    writeFile f\"{n}\" \"x\""
        , "    putStrLn \"one\")"
        , "  writeFile \"e\" \"x\"" ])
        `shouldBe` Right
          [ "d.lam:2:3: " <> droppedText, "d.lam:8:5: " <> droppedText ]
    -- 「🗑️ は、パターンの _ と同じ」
    it "reads 🗑️ as _ in any pattern" $
      fmap snd (loadPlain preludeSession "w.lam"
        "f 🗑️ = case Just 3 of\n  | Just 🗑️ -> 1\n  | 🗑 -> 2\n\
        \(\\🗑️ -> f 0) 9\n")
        `shouldBe` Right (Just "1 : Int")
    -- ANCHOR_END: droppedTest
    -- ANCHOR: orderProp
    -- 「いちばん外の定義は、どの順に並べても同じ答え」
    it "gives the same answer in any order of the definitions" $
      property $ forAll (shuffle parityParts) $ \parts ->
        let source = T.unlines (parts <> ["map steps [1, 6, 27]"])
        in fmap snd (loadPlain preludeSession "parity.lam" source)
             === Right (Just "[0, 8, 111] : [Int]")
    -- ANCHOR_END: orderProp
    -- ANCHOR: whereOrder
    -- 「where の部品も、下の部品を使ってよい」
    it "lets a part of where use a part below it" $
      fmap snd (loadPlain preludeSession "w.lam"
        "double x = twice\n  where\n    twice = once + once\n    once = x\n\
        \double 21\n")
        `shouldBe` Right (Just "42 : Int")
    -- ANCHOR_END: whereOrder
    -- ANCHOR: closedRepl
    -- 「閉じた let は一般に。外の名前を使う let は、注釈がなければ1つの型」
    it "makes a closed let general, and a let with an outer name not" $
      runLines
        [ "let wrap x = display x in wrap 1 ++ wrap \"a\""
        , "(\\n -> let f x = display x ++ display n in f 1 ++ f \"a\") 2"
        , "(\\n -> let f x = display x ++ display n in f 1 ++ f 3) 2" ]
        `shouldBe`
          [ "\"1a\" : String"
          , "type error: the types do not fit: a number where String \
            \is needed"
          , "\"1232\" : String" ]
    -- ANCHOR_END: closedRepl
    -- ANCHOR: nestedCase
    -- 「中の case より左の | は、外の case の枝。1行なら、これまでどおり
    --   中の case の枝」
    it "gives a | left of the inner branches to the outer case" $ do
      let source = T.unlines
            [ "case a of"
            , "  | 1 -> case b of"
            , "      | 2 -> x"
            , "      | _ -> y"
            , "  | _ -> z" ]
      fmap render (parseExpr source) `shouldBe`
        Right "case a of | 1 -> (case b of | 2 -> x | _ -> y) | _ -> z"
      fmap render (parseExpr "case a of | 1 -> case b of | 2 -> x | _ -> y")
        `shouldBe` Right "case a of | 1 -> case b of | 2 -> x | _ -> y"
    -- ANCHOR_END: nestedCase
    -- ANCHOR: keyRepl
    -- 「REPL でも、名前の形のキーに警告する（行は動かす）」
    it "warns about a key in the REPL too, and runs the line" $ do
      let (outs, world) = runLinesIn emptyWorld
            [ "let url = \"/memos\"", "{url: 1}", "{[url]: 1}" ]
      drop 1 outs `shouldBe`
        [ "{\"url\": 1} : Map String Int"
        , "{\"/memos\": 1} : Map String Int" ]
      errorsOf world `shouldBe`
        "warning: the key url is the text \"url\", not the variable url \
        \(write {[url]: …} for the variable, or {\"url\": …} to keep \
        \the text)\n"
    -- ANCHOR_END: keyRepl
    -- ANCHOR: answerRing
    -- 「ステップ1：注釈を書けば、おたがいを呼ぶ関数にも辞書が渡る」
    golden "hardening/answers/ring"
    -- ANCHOR_END: answerRing
    -- ANCHOR: answerShadow
    -- 「ステップ2：いちばん外の名前を隠すローカルな名前」
    it "warns about a local name that hides a name at the top" $ do
      let found = do
            (decls, _) <- either (Left . T.pack . show) Right
              (runLamParser ops programParts "s.lam"
                 "total = 3\nf total = total + 1\n")
            pure [ w | DefValue _ e <- decls
                     , w <- shadowWarnings (Set.fromList ["total", "f"]) e ]
      found `shouldBe` Right
        [ "warning: the local total hides the definition total at \
          \the top" ]
    -- ANCHOR_END: answerShadow
    -- ANCHOR: answerGroups
    -- 「ステップ3：lam が確かめるグループの順」
    it "tells the groups in the order they are checked" $
      groupNames (T.unlines parityParts)
        `shouldBe` Right [["isEven", "isOdd"], ["next"], ["steps"]]
    -- ANCHOR_END: answerGroups

  describe "program" $
    -- ANCHOR: layout
    -- 「1桁目から始まる行は、新しい定義（か、最後の式）」
    it "starts a new definition at column 1" $
      fmap (\(decls, final) ->
              ([ x | DefValue x _ <- decls ], fmap render final))
        (runLamParser ops programParts "" "let a = 1\nlet b =\n  a + 1\nb\n")
        `shouldBe` Right (["a", "b"], Just "b")
    -- ANCHOR_END: layout

-- 🎁 The type of a match, as the REPL shows it: the record, with its
--    fields in the order of names (Match is only another name for it).
-- 見つけたものの型を、REPL の見せ方で（Match は、その別名）
matchType :: Text
matchType = "{ groups : [String], named : Map String String, \
            \start : Int, text : String }"

-- ANCHOR: genDecode
-- 🎲 An item of a JSON array: a whole number, or a string.
-- JSON の配列の要素：整数か、文字列
genIntOrText :: Gen (Either Integer Text)
genIntOrText = oneof [Left <$> arbitrary, Right <$> genText]

itemJson :: Either Integer Text -> Text
itemJson = either (T.pack . show) (decodeUtf8 . BL.toStrict . A.encode)

-- 🎲 A header (different names, not empty) and rows as long as it.
--    The fields are any strings, quotes and line breaks too.
-- 見出し（ちがう名前、空でない）と、それと同じ長さの行。フィールドは
-- どんな文字でもよい（引用符や改行も）
genTable :: Gen ([Text], [[Text]])
genTable = do
  n <- choose (1, 4)
  header <- take n . nub <$> listOf1 (elements ["a", "b", "λ", "q r", "\""])
  rows <- listOf1 (vectorOf (length header) genText
                     `suchThat` (/= [""]))
  pure (header, rows)
-- ANCHOR_END: genDecode

-- ANCHOR: genCalc
-- 🎲 A calculation and its value, written with spaces here and there,
--    and ( ) around some of the parts. No / (0 would stop the test).
-- 計算と、その値。空白をところどころに入れ、部品のいくつかを ( ) で
-- 包む。/ は使わない（0 で割ると止まるため）
genCalc :: Gen (Text, Integer)
genCalc = sized (\n -> go (min n 6))
  where
    go :: Int -> Gen (Text, Integer)
    go 0 = number
    go n = frequency
      [ (1, number)
      , (3, do
          (a, x) <- go (n `div` 2)
          (b, y) <- go (n `div` 2)
          (o, f) <- elements [("+", (+)), ("-", (-)), ("*", (*))]
          sp <- elements ["", " ", "  "]
          pure ("(" <> a <> sp <> o <> sp <> b <> ")", f x y)) ]
    number = do
      k <- choose (0, 99)
      pure (T.pack (show k), k)

-- 🖨️ A number as an argument of Just: Just 3, Just (-3).
-- Just の引数としての数
justArgument :: Integer -> Text
justArgument v
  | v < 0     = "(" <> T.pack (show v) <> ")"
  | otherwise = T.pack (show v)
-- ANCHOR_END: genCalc

-- ANCHOR: genRegexText
-- 🎲 Some regexes, and texts made of the letters they look for (and of
--    U+E000, the first letter the cutting could use as its mark).
-- いくつかの正規表現と、それが探す文字でできた文字列（切るときの印の
-- 候補 U+E000 も混ぜる）
regexes :: [Text]
regexes =
  ["\\d+", "a*", "[λラ]", "\\s", "(?<=a)b", "", "(a)|(b)", "^", "$", "b+?"]

genRegexText :: Gen Text
genRegexText = T.pack <$> listOf (elements "ab1 λラ\n\xE000\xE001")

-- 🪡 a1 b1 a2 b2 ... an (one more a than b).
-- 交互に並べる（a のほうが1つ多い）
zipLonger :: [Text] -> [Text] -> [Text]
zipLonger as bs = case (as, bs) of
  (a : as', b : bs') -> a : b : zipLonger as' bs'
  _                  -> as
-- ANCHOR_END: genRegexText

-- 📜 A program as its declarations and its last expression.
-- プログラムを、宣言と最後の式の組に
programParts :: Parser ([Decl], Maybe Expr)
programParts = (\p -> (programDecls p, programFinal p)) <$> program

-- ANCHOR: runModules
-- 🗂️ Load main.lam with these files (the modules) around it: what
--    the expression at the end shows, or the error.
-- この（モジュールの）ファイルを置いて、main.lam を読みこむ。最後の式が
-- 見せるものか、エラー
runModules :: [(FilePath, Text)] -> Text -> Either Text Text
runModules files source =
  fmap (fromMaybe "" . snd) $ (>>= finalText) $ runIdentity $
    loadProgram (mapFiles (Map.fromList files) []) preludeSession
      preludeSession "main.lam" source

-- 🗂️ The same, with Shapes.lam.
-- 同じく、Shapes.lam を置いて
withShapes :: Text -> Either Text Text
withShapes = runModules [("Shapes.lam", shapes)]

-- 🔷 A module with a type, a class, two instances, and a secret.
-- 型、クラス、インスタンス2つ、それに秘密のあるモジュール
shapes :: Text
shapes = T.unlines
  [ "module Shapes exposing (Shape (..), Describe (..), area, square)"
  , "data Shape = Circle Int | Rect Int Int"
  , "class Describe a where"
  , "  describe : a -> String"
  , "instance Describe Shape where"
  , "  describe s = case s of | Circle r -> f\"circle {r}\""
  , "    | Rect w h -> f\"rect {w} {h}\""
  , "instance Display Shape where"
  , "  display s = f\"({describe s})\""
  , "area s = case s of | Circle r -> 3 * r * r | Rect w h -> w * h"
  , "square n = Rect n n"
  , "secret = 42" ]
-- ANCHOR_END: runModules

-- ⌨️ REPL lines, where an import line looks in these files.
-- REPL の行。import の行は、このファイルから探す
replImporting :: [(FilePath, Text)] -> [Text] -> IO [Text]
replImporting files inputs = pure (snd (foldl go (preludeSession, []) inputs))
  where
    go (s, outs) line = case parseInput (sessionOperators s) line of
      Right (ImportLine imp) ->
        case runIdentity (importAt (mapFiles (Map.fromList files) [])
                            preludeSession s imp) of
          Left err -> (s, outs <> [err])
          Right s' -> (s', outs <> [""])
      _ -> let (s', out) = replStep s line in (s', outs <> [out])

-- ANCHOR: replAfter
-- 📂 :load a file, then answer some REPL lines.
-- ファイルを :load してから、REPL の行に答える
replAfter :: FilePath -> [Text] -> IO [Text]
replAfter file inputs = do
  source <- Utf8.readFile file
  let loaded = case loadPlain preludeSession file source of
        Right (s, _) -> s
        Left err     -> error (T.unpack err)
      go (s, outs) line = let (s', out) = replStep s line in (s', outs <> [out])
  pure (snd (foldl go (loaded, []) inputs))

-- 🖨️ A list of numbers as Lam source (negative ones in brackets).
-- 数のリストを Lam のソースに（負の数は括弧で）
lamList :: [Integer] -> Text
lamList xs = "[" <> T.intercalate ", " (map num xs) <> "]"
-- ANCHOR_END: replAfter

-- ANCHOR: genMaybe
-- 🎲 Nothing, or Just a small number (a negative one, too).
-- Nothing か、小さな数の Just（負の数も）
genMaybe :: Gen (Maybe Integer)
genMaybe = oneof [pure Nothing, Just <$> choose (-3, 3)]

-- 🖨️ Maybe and Bool, as Lam writes them.
-- Maybe と Bool を、Lam の書き方で
lamMaybe :: Maybe Integer -> Text
lamMaybe m = maybe "Nothing" (\n -> "Just " <> num n) m

lamBool :: Bool -> Text
lamBool b = if b then "true" else "false"
-- ANCHOR_END: genMaybe

-- 🖨️ A list of numbers, as Lam writes it: [1, 2, 3]
-- 数のリストを、Lam の書き方で
renderList :: [Integer] -> Text
renderList xs = "[" <> T.intercalate ", " (map (T.pack . show) xs) <> "]"

-- ANCHOR: inRecord
-- 📇 let r = <record> in <expression>, for the REPL.
-- REPL のため
inRecord :: Text -> Text -> Text
inRecord r e = "let r = " <> r <> " in " <> e

-- 🔢 A number as Lam source code (a negative one is (-5): the minus
--    right before the number), and the value part of an answer.
-- 数を Lam のソースに（負の数は (-5)）。答えから、値の部分を取り出す
num :: Integer -> Text
num n
  | n < 0     = "(" <> T.pack (show n) <> ")"
  | otherwise = T.pack (show n)

valuePart :: Text -> Text
valuePart = T.strip . fst . T.breakOn " : "
-- ANCHOR_END: inRecord

-- ANCHOR: genResult
-- 🎲 Ok with a small number, or Err with a short word.
-- 小さな数の Ok か、短い言葉の Err
genResult :: Gen (Either Text Integer)
genResult = oneof [ Right <$> choose (-3, 3)
                  , Left <$> elements ["no", "bad", "oops"] ]

-- 🖨️ A Result as Lam writes it, and a list of them as the REPL shows.
-- Result を Lam の書き方で。リストの Result を REPL の見せ方で
lamResult :: Either Text Integer -> Text
lamResult r = case r of
  Right n -> "Ok " <> num n
  Left e  -> "Err \"" <> e <> "\""

lamResultList :: Either Text [Integer] -> Text
lamResultList r = case r of
  Right ns -> "Ok [" <> T.intercalate ", " (map (T.pack . show) ns) <> "]"
  Left e   -> "Err \"" <> e <> "\""

-- 🏷️ A name with a number:  named "x" 2  is  x2
-- 番号の付いた名前
named :: Text -> Int -> Text
named x i = x <> T.pack (show i)
-- ANCHOR_END: genResult

-- ANCHOR: cliHelpers
-- 🧭 Run an example of the command line with these words, in the fake
--    world: what it writes, then its errors, then [exit n].
-- コマンドラインの例を、この語で偽物の世界で動かす：出力、エラー、
-- [exit n] の順
cliRun :: FilePath -> [Text] -> IO Text
cliRun base ws = do
  dbs <- memoryDatabases
  fst <$> runFlagged dbs (\w -> w { worldArgs = ws }) [] (base <> ".lam")

-- 🧭 The same, with each line of <name>.cmd, one after another in the
--    same fake world (todo.txt stays): the flags of lam before the
--    file, the words after it. A line that starts with "@ " gets ready
--    (its output is not kept). Then compare with the .out file that
--    the real lam made (make-cli-out.sh). An error stops lam with the
--    code 1, which the real lam shows as [exit 1].
-- 同じだが、<名前>.cmd の行を、同じ偽物の世界で順に動かす（todo.txt は
-- 残る）。ファイルより前は lam のフラグ、後ろは語。「@ 」で始まる行は
-- 下ごしらえ（出力は残さない）。本物の lam が作った .out とくらべる。
-- エラーは終了コード 1 で、本物の lam では [exit 1] と出る
cliGolden :: String -> Spec
cliGolden name = it ("examples/cli/" <> name <> ".cmd") $ do
  cmd  <- Utf8.readFile ("examples/cli/" <> name <> ".cmd")
  want <- Utf8.readFile ("examples/cli/" <> name <> ".out")
  dbs  <- memoryDatabases
  -- The .lam and .txt files of the folder, as make-cli-out.sh copies
  -- them: poem.txt is read as it is named.
  -- フォルダーの .lam と .txt のファイル（make-cli-out.sh が写すもの）
  names <- listDirectory "examples/cli"
  start <- Map.fromList <$> sequence
    [ (\t -> (T.pack n, t)) <$> B.readFile ("examples/cli/" <> n)
    | n <- names, takeExtension n `elem` [".lam", ".txt"] ]
  let step (shown, files) line = do
        let (keep, rest) = maybe (True, line) (\r -> (False, r))
                             (T.stripPrefix "@ " line)
        (flags, file, ws) <- either fail pure (cmdWords rest)
        (ending, world) <- runFlaggedEnding dbs
          (\w -> w { worldArgs = ws, worldFiles = files <> worldFiles w })
          flags ("examples/cli/" <> file)
        let out = outputOf world <> errorsOf world <> endingLines ending
        pure (if keep then shown <> out else shown, worldFiles world)
  (got, _) <- foldM step ("", start) (T.lines cmd)
  -- lam runs in the folder: its errors name bad-alias.lam, not the path.
  -- lam はフォルダーの中で動くので、エラーは道ではなく bad-alias.lam と言う
  T.replace "examples/cli/" "" got `shouldBe` want
  where
    endingLines ending = case ending of
      Finished _   -> ""
      Errored t    -> t <> "\n[exit 1]\n"
      ExitedWith 0 -> ""
      ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n"

-- 🧭 The words of a line of a .cmd: lam, its flags, the file, then the
--    words for the script ("Buy milk" in quotes is one word).
-- .cmd の1行の語：lam、そのフラグ、ファイル、スクリプトへの語
--（"Buy milk" のように " で囲めば1語）
cmdWords :: Text -> Either String ([String], FilePath, [Text])
cmdWords line = case shellWords line of
  "lam" : more -> case break (".lam" `T.isSuffixOf`) more of
    (flags, file : ws) -> Right (map T.unpack flags, T.unpack file, ws)
    _                  -> Left ("no .lam file: " <> T.unpack line)
  _ -> Left ("a bad .cmd line: " <> T.unpack line)
  where
    shellWords t = case T.uncons (T.stripStart t) of
      Nothing -> []
      Just ('"', more) ->
        let (w, rest) = T.breakOn "\"" more
        in w : shellWords (T.drop 1 rest)
      Just _ ->
        let (w, rest) = T.break (== ' ') (T.stripStart t)
        in w : shellWords rest
-- ANCHOR_END: cliHelpers

-- ANCHOR: cliOf
-- 🧭 The command line of an example, made as lam makes it.
-- 例のコマンドラインを、lam と同じように作る
cliOf :: FilePath -> IO Cli
cliOf base = do
  let file = base <> ".lam"
  source <- Utf8.readFile file
  case runIdentity (loadProgram noFiles preludeSession preludeSession
                      file source) of
    Left err     -> fail (T.unpack err)
    Right (s, _) -> either (fail . T.unpack) pure (commandLine s)
-- ANCHOR_END: cliOf

-- 🐚 An example of the chapter on shells that needs no git, in the fake
--    world: the same as the .out that the real lam made.
-- git の要らないシェルの章の例を、偽物の世界で。本物の lam が作った .out と同じ
shellGolden :: String -> Spec
shellGolden name = it ("examples/shell/" <> name <> ".lam") $ do
  source <- Utf8.readFile ("examples/shell/" <> name <> ".lam")
  want   <- Utf8.readFile ("examples/shell/" <> name <> ".out")
  let (_, w) = script emptyWorld source
  outputOf w `shouldBe` want

-- ANCHOR: oneLinerGolden
-- 🧪 Run examples/oneliner/<name>.cmd (the words after lam) in a fake
--    world: the files of the folder are there, the keyboard types
--    <name>.in, and Shop.lam is found as LAM_PATH=examples/oneliner
--    finds it. Then the standard output, the standard error, how it
--    ended, and each file that -i changed or made (as the real
--    make-oneliner-out.sh shows them).
-- examples/oneliner/<名前>.cmd（lam の後ろの語）を偽物の世界で動かす。
-- フォルダーのファイルを置き、キーボードは <名前>.in を打ち、Shop.lam は
-- LAM_PATH=examples/oneliner と同じく見つける。答えは、標準出力・
-- 標準エラー・終わり方と、-i が変えたか作ったファイル
runOneLinerExample :: FilePath -> IO Text
runOneLinerExample base = do
  cmd <- Utf8.readFile (base <> ".cmd")
  case readCommand (oneLinerWords cmd) Nothing of
    -- A flag lam does not know: how to use it, and the exit code 2.
    -- lam の知らないフラグ：使い方と、終了コード 2
    Left why -> pure (why <> "\n" <> usage <> oneLinerUsage <> "[exit 2]\n")
    Right _  -> runKnown base

runKnown :: FilePath -> IO Text
runKnown base = do
  (opts, one, rest, world) <- exampleWorld base
  files <- exampleFiles
  dbs <- memoryDatabases
  guard <- guarded opts
  (ending, after) <- runFakeIO world { worldArgs = map T.pack rest }
    (runOneLiner (guard (fakeWorldIO dbs)) files preludeSession one)
  pure (shownRun world ending after)

-- 📚 The modules of the examples, with LAM_PATH=examples/oneliner.
-- 例のモジュール。LAM_PATH=examples/oneliner と同じ
exampleFiles :: Monad m => IO (Files m)
exampleFiles = do
  modules <- lamFiles "examples/oneliner"
  let table = Map.fromList modules
  pure (Files (\f -> pure (Map.lookup f table)) ["examples/oneliner"])

-- 🎭 The flags, the one-liner, the words after it, and the fake world
--    of an example.
-- 例のフラグ・ワンライナー・後ろの語・偽物の世界
exampleWorld :: FilePath -> IO (Options, OneLiner, [String], FakeWorld)
exampleWorld base = do
  cmd   <- Utf8.readFile (base <> ".cmd")
  there <- doesFileExist (base <> ".in")
  input <- if there then T.lines <$> Utf8.readFile (base <> ".in")
                    else pure []
  let dir = takeDirectory base
  names <- listDirectory dir
  files <- mapM (\n -> (,) (T.pack (dir </> n)) <$> B.readFile (dir </> n))
             [ n | n <- names
                 , takeExtension n `notElem` [".cmd", ".out", ".in", ".git"] ]
  (opts, command) <- either (fail . T.unpack) pure
                       (readCommand (oneLinerWords cmd) Nothing)
  (one, rest) <- case command of
    RunOneLiner one rest -> pure (one, rest)
    other                -> fail ("not a one-liner: " <> show other)
  pure (opts, one, rest, emptyWorld { worldInput = input
                                    , worldFiles = Map.fromList files })

-- 🖥️ How a run looks: the standard output, the standard error, how it
--    ended, and each file that it changed or made.
-- 動かした様子：標準出力・標準エラー・終わり方・変えたか作ったファイル
shownRun :: FakeWorld -> Ending -> FakeWorld -> Text
shownRun world ending after =
  outputOf after <> errorsOf after <> shown <> T.concat changed
  where
    shown = case ending of
      Finished ""  -> ""
      Finished out -> out <> "\n"
      Errored err  -> err <> "\n[exit 1]\n"
      ExitedWith 0 -> ""
      ExitedWith n -> "[exit " <> T.pack (show n) <> "]\n"
    changed = [ "== " <> f <> " ==\n" <> decodeUtf8 t
              | (f, t) <- Map.toList (worldFiles after)
              , Map.lookup f (worldFiles world) /= Just t ]

-- ✂️ The words of a command line: spaces cut, '...' and "..." keep
--    them (as sh reads the .cmd files).
-- コマンドの行の語：空白で切る。'...' と "..." の中は切らない
oneLinerWords :: Text -> [String]
oneLinerWords = go . T.unpack . T.strip
  where
    go s = case dropWhile (== ' ') s of
      ""  -> []
      s'  -> let (w, rest) = word s' in w : go rest
    word s = case s of
      []           -> ("", [])
      ' ' : rest   -> ("", rest)
      '\'' : rest  -> quoted '\'' rest
      '"' : rest   -> quoted '"' rest
      c : rest     -> let (w, r) = word rest in (c : w, r)
    quoted q rest =
      let (inside, after) = break (== q) rest
          (w, r) = word (drop 1 after)
      in (inside <> w, r)

-- 🖨️ What -pe 'program' prints for these lines, in a fake world.
-- これらの行に、-pe 'プログラム' が出すもの（偽物の世界で）
printed :: Text -> [Text] -> IO Text
printed program ls =
  printedFrom (T.unlines ls)
    noOneLiner { oneProgram = [program], oneMode = Printing }

-- 🖨️ What a one-liner prints for this standard input.
-- この標準入力に、ワンライナーが出すもの
printedFrom :: Text -> OneLiner -> IO Text
printedFrom text one = do
  dbs <- memoryDatabases
  (_, w) <- runFakeIO emptyWorld { worldInput = T.lines text }
    (runOneLiner (fakeWorldIO dbs) noFiles preludeSession one)
  pure (outputOf w <> errorsOf w)

-- 🎲 A line of a, b and spaces (so that s/a+/.../g finds something).
-- a・b・空白の行（s/a+/…/g が見つけるものがあるように）
genLine :: Gen Text
genLine = T.pack <$> listOf (elements "aab ")
-- ANCHOR_END: oneLinerGolden

-- ANCHOR: realRun
-- 🌍 Run a script in the real world (threads, real naps): how it
--    ended, and how long it took in seconds. The modules are given.
-- 本物の世界（スレッド、本物の昼寝）でスクリプトを動かす：どう終わったか
-- と、何秒かかったか。モジュールは渡す
realRunWith :: [(FilePath, Text)] -> Text -> IO (Ending, Double)
realRunWith modules source = do
  dice <- newDice (Just 618)
  desk <- newDesk
  withOutside $ \outside -> do
    start <- getCurrentTime
    let loaded = runIdentity (loadProgram (mapFiles (Map.fromList modules) [])
                   preludeSession preludeSession "real.lam" source)
    ending <- case loaded of
      Left err            -> pure (Errored err)
      Right (done, final) -> runProgram (realWorld dice desk outside) done final
    end <- getCurrentTime
    pure (ending, realToFrac (diffUTCTime end start))

realRun :: Text -> IO (Ending, Double)
realRun = realRunWith []

-- 🖨️ Numbers, as Lam writes a list of them: [3, 1, 2]
-- 数のリストを、Lam の書き方で
lamInts :: [Integer] -> Text
lamInts ns = "[" <> T.intercalate ", " (map (T.pack . show) ns) <> "]"
-- ANCHOR_END: realRun

-- 🔁 The definitions of examples/hardening/parity.lam, one string each.
-- examples/hardening/parity.lam の定義。1つずつ文字に
parityParts :: [Text]
parityParts =
  [ "isEven n = if n == 0 then true else isOdd (n - 1)"
  , "isOdd n = if n == 0 then false else isEven (n - 1)"
  , "steps : Int -> Int\nsteps n = if n == 1 then 0 else 1 + steps (next n)"
  , "next : Int -> Int\nnext n = if isEven n then n // 2 else 3 * n + 1" ]
