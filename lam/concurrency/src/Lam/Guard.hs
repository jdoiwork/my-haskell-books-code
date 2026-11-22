-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Guards around a handler: permissions (--allow-net=pokeapi.co, ...),
--   --trace and --dry-run. Each one takes a handler and gives a handler,
--   so they work the same in the real world and in a fake one.
-- ハンドラーをくるむ見張り：権限（--allow-net=pokeapi.co など）、
-- --trace、--dry-run。どれも、ハンドラーを受け取ってハンドラーを返すので、
-- 本物の世界でも偽物の世界でも、同じように効く。
module Lam.Guard
  ( Permission (..)
  , Scope (..)
  , Permissions (..)
  , needs
  , hostOf
  , globRoot
  , covers
  , allowOnly
  , refused
  , Notebook
  , newTracer
  , traced
  , traceLine
  , dryRun
  , changes
  , Options (..)
  , defaultOptions
  , readOptions
  , usage
  , guarded
  ) where

import qualified Data.ByteString as B
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H
import System.FilePath (joinPath, normalise, splitDirectories)
import Text.Read (readMaybe)

import Lam.Builtins (con)
import Lam.Eval
import Lam.Io (Handler)
import Lam.Pretty (renderString)
import Lam.Shell (renderCommand)
import Lam.Utf8 (Decoding (..))
-- ANCHOR_END: head

-- ANCHOR: permission
-- 🔑 What a request may need to be allowed.
-- 頼みごとが、許してもらう必要のあるもの
data Permission = Net | Read | Write | Env | Run
  deriving (Eq, Ord, Show, Enum, Bounded)

-- 🔑 What one permission covers: everything, or only these hosts,
--    folders, names, or commands.
-- 1つの権限が及ぶ範囲：全部か、これらのホスト・フォルダー・名前・
-- コマンドだけか
data Scope = Everything | Within [Text]
  deriving (Eq, Show)

-- 🔑 Everything (no flags), or only the permissions in the Map.
-- 全部（フラグなし）か、Map にある権限だけか
data Permissions = AllowAll | Only (Map Permission Scope)
  deriving (Eq, Show)
-- ANCHOR_END: permission

-- ANCHOR: needs
-- 🔑 The permissions a request needs, each with what it is about:
--    the host of a URL, the path of a file, the name of a variable,
--    the name of a command. Every request is written here, with no
--    "_ ->", so a new request cannot be forgotten
--    (-Wincomplete-patterns tells us).
-- 頼みごとに要る権限と、その相手：URL のホスト、ファイルの道、変数の
-- 名前、コマンドの名前。どの頼みごとも、"_ ->" なしでここに書くので、
-- 新しい頼みごとを書き忘れない（-Wincomplete-patterns が教える）
needs :: Request -> [(Permission, Text)]
needs request = case request of
  -- The screen, the keyboard, and the arguments: nothing.
  -- 画面・キーボード・引数：何も要らない
  PutStr _               -> []
  PutErr _               -> []
  GetLine                -> []
  GetContents            -> []
  GetArgs                -> []
  -- The environment: its name. (getEnvironment: see allowOnly.)
  -- 環境変数：その名前（getEnvironment は allowOnly で）
  LookupEnv name         -> [(Env, name)]
  GetEnvironment         -> []
  -- Reading a file or a folder needs --allow-read.
  -- ファイルやフォルダーを読むには --allow-read
  ReadFile path          -> [(Read, path)]
  ReadBytes path         -> [(Read, path)]
  -- "-" is the standard input: as getLine, it needs nothing.
  -- "-" は標準入力。getLine と同じく、何も要らない
  OpenFile "-" _         -> []
  OpenFile path _        -> [(Read, path)]
  ListDir dir            -> [(Read, dir)]
  Glob pattern           -> [(Read, globRoot pattern)]
  Exists path            -> [(Read, path)]
  IsDir path             -> [(Read, path)]
  FileInfo path          -> [(Read, path)]
  -- A file opened already: it was asked when it was opened.
  -- もう開いたファイル：開くときに聞いた
  ReadLine _             -> []
  ReadChunk _ _          -> []
  CloseFile _            -> []
  -- Changing needs --allow-write (both names, for rename).
  -- 変えるには --allow-write（rename は、前と後の両方の名前）
  WriteFile path _       -> [(Write, path)]
  WriteBytes path _      -> [(Write, path)]
  AppendFile path _      -> [(Write, path)]
  MakeDir dir            -> [(Write, dir)]
  Remove path            -> [(Write, path)]
  Rename from to         -> [(Write, from), (Write, to)]
  CopyFile from to       -> [(Read, from), (Write, to)]
  -- A command needs --allow-run, with its name.
  -- コマンドには --allow-run。相手はその名前
  RunProcess command _ _ -> [(Run, command)]
  -- ANCHOR: needsShell
  RunCommand c           -> [(Run, commandName c)]
  -- The terminal: nothing.
  -- 端末：何も要らない
  IsTerminal             -> []
  GetSecret              -> []
  -- ANCHOR_END: needsShell
  -- The clock and the dice: nothing.
  -- 時計とさいころ：何も要らない
  Now                    -> []
  Here                   -> []
  RandomInt _ _          -> []
  -- ANCHOR: needsSleep
  -- Waiting, and the time since the start: nothing.
  -- 待つことと、始まってからの時間：何も要らない
  Sleep _                -> []
  Uptime                 -> []
  -- ANCHOR_END: needsSleep
  -- The web: the host. SQLite: the file (":memory:" is no file).
  -- Web：ホスト。SQLite：ファイル（":memory:" はファイルではない）
  HttpSend _ url _ _     -> [(Net, hostOf url)]
  SqlOpen ":memory:" _   -> []
  SqlOpen file ReadOnly  -> [(Read, file)]
  SqlOpen file ReadWrite -> [(Read, file), (Write, file)]
  SqlQuery {}            -> []
  SqlExec {}             -> []
  SqlClose _             -> []

-- 🌐 The host of a URL, read by http-client itself, so that it is the
--    host the request will really go to. Not a URL: the text itself.
-- URL のホスト。http-client 自身に読ませるので、本当に行く先になる。
-- URL でなければ、その文字のまま
hostOf :: Text -> Text
hostOf url =
  maybe url (TE.decodeUtf8 . H.host) (H.parseRequest (T.unpack url))

-- 🌳 The folder a glob pattern looks in: the pieces before the first
--    one with * ? or [. "logs/2026-*/*.log" looks in "logs".
-- glob の型が探すフォルダー：* ? [ のある最初のかけらより前。
-- "logs/2026-*/*.log" は "logs" の中を探す
globRoot :: Text -> Text
globRoot pattern =
  let fixed = takeWhile (not . any (`elem` ("*?[" :: String)))
                (splitDirectories (T.unpack pattern))
  in if null fixed then "." else T.pack (joinPath fixed)
-- ANCHOR_END: needs

-- ANCHOR: covers
-- 🎯 Does a scope cover this host, path, name, or command?
--    A folder covers the paths inside it, piece by piece: "data" covers
--    "data/a.txt" but not "data2/a.txt". A path with ".." is covered
--    only by Everything: "data/../secret.txt" goes out of data.
-- 範囲が、このホスト・道・名前・コマンドに及ぶか。フォルダーは、その中の
-- 道に及ぶ。くらべるのは区切りごと："data" は "data/a.txt" に及ぶが、
-- "data2/a.txt" には及ばない。".." のある道に及ぶのは Everything だけ：
-- "data/../secret.txt" は data の外へ出る
covers :: Permission -> Scope -> Text -> Bool
covers _ Everything _ = True
covers permission (Within allowed) target = case permission of
  Read  -> any (inside target) allowed
  Write -> any (inside target) allowed
  _     -> target `elem` allowed
  where
    inside path folder =
      let parts = pieces path
          top   = pieces folder
      in ".." `notElem` parts && take (length top) parts == top
    -- "./data/a.txt" is ["data", "a.txt"], and "." is []: the folder
    -- where lam was started. / "." は、lam を動かしたフォルダー
    pieces = filter (/= ".") . splitDirectories . normalise . T.unpack
-- ANCHOR_END: covers

-- ANCHOR: allowOnly
-- 🚧 Ask the handler only when every permission the request needs is
--    given, and covers what it is about. Otherwise do not ask: answer
--    as refused says. getEnvironment is asked, and only the names
--    allowed are kept.
-- 要る権限が全部あり、相手に及ぶときだけ、ハンドラーに頼む。なければ
-- 頼まずに、refused の言うとおりに答える。getEnvironment は頼んで、
-- 許した名前だけを残す
allowOnly :: Monad m => Permissions -> Handler m -> Handler m
allowOnly AllowAll handle = handle
allowOnly (Only given) handle = \request ->
  case filter (not . uncurry allowed) (needs request) of
    []          -> keepAllowed request <$> handle request
    missing : _ -> pure (refused request (needsFlag missing))
  where
    allowed p what = maybe False (\scope -> covers p scope what)
                       (Map.lookup p given)
    keepAllowed GetEnvironment (VMap env) =
      VMap (Map.filterWithKey (\k _ -> allowedName k) env)
    keepAllowed _ answer = answer
    allowedName (Key (VString k)) = allowed Env k
    allowedName _                 = False

-- 🚩 "needs --allow-net=pokeapi.co": the flag to add, as it is.
-- 足せば通るフラグを、そのままの形で
needsFlag :: (Permission, Text) -> Text
needsFlag (p, what) = "needs " <> flag <> "=" <> what
  where
    flag = case p of
      Net   -> "--allow-net"
      Read  -> "--allow-read"
      Write -> "--allow-write"
      Env   -> "--allow-env"
      Run   -> "--allow-run"
-- ANCHOR_END: allowOnly

-- ANCHOR: refused
-- 🙅 The answer to a request that is not allowed. The ones that touch a
--    file or the network answer with a Result, so it is an Err with the
--    flag to add. lookupEnv says Nothing, and File.exists and isDir say
--    false: as if it were not there. A command does not start.
-- 許されない頼みごとへの答え。ファイルやネットに触れる頼みごとは Result
-- で答えるので、足すフラグを書いた Err。lookupEnv は Nothing、
-- File.exists と isDir は false：ないのと同じ。コマンドは始まらない
refused :: Request -> Text -> Value
refused request why = case request of
  LookupEnv _        -> con "Nothing" []
  Exists _           -> VBool False
  IsDir _            -> VBool False
  RunProcess c _ _   -> con "CannotStart" [VString (c <> ": " <> why)]
  -- ANCHOR: refusedShell
  RunCommand c       ->
    con "CannotStart" [VString (commandName c <> ": " <> why)]
  -- ANCHOR_END: refusedShell
  HttpSend _ url _ _ -> err url
  Rename from _      -> err from
  CopyFile from _    -> err from
  _                  -> err (subject (needs request))
  where
    err what = con "Err" [VString (what <> ": " <> why)]
    subject ns = case ns of
      (_, what) : _ -> what
      []            -> "lam"
-- ANCHOR_END: refused

-- ANCHOR: traced
-- 📓 The notebook of a tracer: how many times each open file was read.
-- 見張りのノート：開いているファイルごとに、何回読んだか
type Notebook = IORef (Map Int Int)

-- 👣 A new tracer, with an empty notebook.
-- 新しい見張り。ノートは空
newTracer :: MonadIO m => IO (Handler m -> Handler m)
newTracer = traced <$> newIORef Map.empty

-- 👣 Write each request to the standard error before doing it. It uses
--    the same handler (PutErr), so a fake world keeps the trace, too.
--    The lines of a file read one by one are only counted, and the
--    count is told when the file is closed.
-- 頼みごとを、やる前に標準エラーに書く。同じハンドラー（PutErr）で書く
-- ので、偽物の世界にも記録が残る。1行ずつ読むファイルの行は数える
-- だけにして、閉じるときに数を言う
traced :: MonadIO m => Notebook -> Handler m -> Handler m
traced notebook handle request = case request of
  ReadLine n    -> count n >> handle request
  ReadChunk n _ -> count n >> handle request
  CloseFile n   -> do
    times <- liftIO $ atomicModifyIORef' notebook $ \m ->
      (Map.delete n m, Map.findWithDefault 0 n m)
    say (traceLine request <> " (" <> number times <> " reads)")
    handle request
  -- The number of an open file is known after it is opened.
  -- 開いたファイルの番号は、開いたあとでわかる
  OpenFile _ _  -> do
    answer <- handle request
    say (traceLine request <> handleOf answer)
    pure answer
  _ -> say (traceLine request) >> handle request
  where
    count n = liftIO (atomicModifyIORef' notebook
                (\m -> (Map.insertWith (+) n 1 m, ())))
    -- The screen is not shown: it is on the screen already. Each line
    -- starts with the seconds since the program started, so the jobs
    -- that start at once show the same time.
    -- 画面に書くのは見せない（もう画面に出ている）。行の頭には、
    -- プログラムが始まってからの秒数。同時に始まった仕事は、同じ時間で
    -- 並ぶ
    -- ANCHOR: tracedTime
    say line = case request of
      PutStr _ -> pure ()
      PutErr _ -> pure ()
      Uptime   -> pure ()
      _        -> do
        ms <- handle Uptime
        () <$ handle (PutErr ("trace " <> seconds ms <> ": " <> line <> "\n"))
    -- 1234 ms is "1.234s"
    -- 1234 ミリ秒は "1.234s"
    seconds v = case v of
      VInt ms -> let (whole, part) = ms `divMod` 1000
                 in number whole <> "." <> T.justifyRight 3 '0' (number part)
                      <> "s"
      _       -> "?"
    -- ANCHOR_END: tracedTime
    handleOf (VCon "Ok" _ [VCon "Handle" _ [VInt n]]) = " -> #" <> number n
    handleOf _ = ""

-- 📝 A request in one line, as a Lam call.
-- 頼みごとを、Lam の呼び出しの形で1行に
traceLine :: Request -> Text
traceLine request = case request of
  PutStr s            -> "putStr " <> quote s
  PutErr s            -> "ePutStr " <> quote s
  GetLine             -> "getLine"
  GetContents         -> "getContents"
  GetArgs             -> "getArgs"
  LookupEnv k         -> "lookupEnv " <> quote k
  GetEnvironment      -> "getEnvironment"
  ReadFile p          -> "readFile " <> quote p
  WriteFile p t       -> "writeFile " <> quote p <> letters t
  ListDir d           -> "File.list " <> quote d
  Glob p              -> "File.glob " <> quote p
  Exists p            -> "File.exists " <> quote p
  IsDir p             -> "File.isDir " <> quote p
  FileInfo p          -> "File.info " <> quote p
  MakeDir d           -> "File.makeDir " <> quote d
  Remove p            -> "File.remove " <> quote p
  Rename a b          -> "File.rename " <> quote a <> " " <> quote b
  CopyFile a b        -> "File.copy " <> quote a <> " " <> quote b
  AppendFile p t      -> "File.append " <> quote p <> letters t
  RunProcess c as i   -> commandLine c as i
  -- ANCHOR: traceShell
  RunCommand c        -> "Shell.run " <> renderCommand c
  IsTerminal          -> "Term.isTerminal"
  GetSecret           -> "Term.readSecret"
  -- ANCHOR_END: traceShell
  Now                 -> "Time.now"
  Here                -> "Time.here"
  RandomInt a b       -> "Random.int " <> number a <> " " <> number b
  Sleep ms            -> "sleep " <> number ms
  Uptime              -> "uptime"
  OpenFile p Strict   -> "File.open " <> quote p
  OpenFile p how      -> "File.openWith " <> T.pack (show how) <> " "
                           <> quote p
  ReadLine n          -> "File.nextLine #" <> number n
  ReadChunk n size    -> "File.nextChunk " <> number size <> " #" <> number n
  CloseFile n         -> "File.close #" <> number n
  ReadBytes p         -> "File.readBytes " <> quote p
  WriteBytes p b      -> "File.writeBytes " <> quote p <> " ("
                           <> number (B.length b) <> " bytes)"
  HttpSend m u _ _    -> "Http.send " <> quote m <> " " <> quote u
  SqlOpen f ReadWrite -> "Sql.open " <> quote f
  SqlOpen f ReadOnly  -> "Sql.openReadOnly " <> quote f
  SqlQuery n s cs     -> "Sql.query #" <> number n <> " " <> quote s
                           <> cells cs
  SqlExec n s cs      -> "Sql.exec #" <> number n <> " " <> quote s
                           <> cells cs
  SqlClose n          -> "Sql.close #" <> number n
  where
    letters t = " (" <> number (T.length t) <> " letters)"
    cells cs = " [" <> T.intercalate ", " (map cell cs) <> "]"
    cell c = case c of
      CNull    -> "JNull"
      CInt i   -> number i
      CDec n d -> "JDec " <> number n <> " " <> number d
      CText s  -> quote s

-- 📝 Process.run "git" ["log"] (or Process.pipe, with the length of
--    the input).
-- Process.run の形で1行に（入力があれば Process.pipe と長さ）
commandLine :: Text -> [Text] -> Text -> Text
commandLine c as i
  | T.null i  = "Process.run " <> quote c <> " " <> args
  | otherwise = "Process.pipe " <> quote c <> " " <> args <> " ("
                  <> number (T.length i) <> " letters)"
  where
    args = "[" <> T.intercalate ", " (map quote as) <> "]"
-- ANCHOR_END: traced

-- 📝 "ピカチュウ", as Lam writes it, and a number.
-- Lam の書き方の文字列と、数
quote :: Text -> Text
quote = renderString

number :: (Show n) => n -> Text
number = T.pack . show

-- ANCHOR: dryRun
-- 🧪 Do what only looks, and only tell what would change: writing and
--    changing files, an HTTP request that is not GET, Sql.exec (BEGIN
--    and COMMIT too), and every command. A database is opened only to
--    read, so even a mistake cannot write to it.
-- 見るだけのことはやり、変えることは知らせるだけにする：ファイルを書く・
-- 変える、GET でない HTTP、Sql.exec（BEGIN と COMMIT も）、どのコマンド
-- も。データベースは読むだけに開くので、まちがえても書けない
dryRun :: Monad m => Handler m -> Handler m
dryRun handle request
  | changes request = do
      _ <- handle (PutErr ("dry-run: " <> traceLine request <> "\n"))
      pure (pretended request)
  | otherwise = handle (onlyToRead request)
  where
    onlyToRead (SqlOpen f ReadWrite) = SqlOpen f ReadOnly
    onlyToRead r = r

-- 🧪 Does the request change the world outside? A command may change
--    anything, so even git log is not run.
-- 頼みごとは、外の世界を変えるか。コマンドは何を変えるかわからないので、
-- git log も動かさない
changes :: Request -> Bool
changes request = case request of
  WriteFile {}       -> True
  WriteBytes {}      -> True
  AppendFile {}      -> True
  MakeDir _          -> True
  Remove _           -> True
  Rename {}          -> True
  CopyFile {}        -> True
  SqlExec {}         -> True
  RunProcess {}      -> True
  -- ANCHOR: changesShell
  RunCommand {}      -> True
  -- ANCHOR_END: changesShell
  HttpSend m _ _ _   -> m `notElem` ["GET", "HEAD"]
  _                  -> False

-- 🧪 What a change answers on a dry run: Ok for files and SQL, as if it
--    were done. A request to the web and a command were not done, and
--    say so.
-- 試しに動かすとき、変えることの答え：ファイルと SQL は、やったことに
-- して Ok。Web とコマンドは、やっていないと言う
pretended :: Request -> Value
pretended request = case request of
  SqlExec {}         -> con "Ok" [VInt 0]
  -- 「試しに動かしているので、送っていない」
  HttpSend m u _ _   ->
    con "Err" [VString ("dry run: " <> m <> " " <> u <> " was not sent")]
  -- 「試しに動かしているので、動かしていない」
  RunProcess c _ _   ->
    con "CannotStart" [VString ("dry run: " <> c <> " was not run")]
  -- ANCHOR: pretendShell
  RunCommand c       -> con "CannotStart"
    [VString ("dry run: " <> commandName c <> " was not run")]
  -- ANCHOR_END: pretendShell
  _                  -> con "Ok" [VTuple []]
-- ANCHOR_END: dryRun

-- ANCHOR: options
-- 🚩 The flags before the file name.
-- ファイルの名前より前のフラグ
data Options = Options
  { permissions :: Permissions
  , tracing     :: Bool
  , dryRunning  :: Bool
  , randomSeed  :: Maybe Int     -- --seed=618: the same dice every time
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options AllowAll False False Nothing

-- 🚩 Read the flags at the head of the arguments; the rest is the file
--    and its arguments. No flag: everything is allowed. --allow-...
--    or --deny-all: only what is allowed. --allow-net=a,b gives only
--    those hosts (folders for read and write, names for env and run).
--    The seed is --seed=618 first, then LAM_SEED (the second argument).
-- 引数の頭のフラグを読む。残りは、ファイルとその引数。フラグなし：
-- 全部許す。--allow-… か --deny-all：許したものだけ。--allow-net=a,b
-- なら、そのホストだけ（read と write はフォルダー、env と run は名前）。
-- 種は、まず --seed=618、次に LAM_SEED（2つ目の引数）
readOptions
  :: [String] -> Maybe String -> Either Text (Options, [String])
readOptions args fromEnv = do
  (opts, rest) <- go defaultOptions args
  seed <- case (randomSeed opts, fromEnv) of
    (Nothing, Just text) -> whole "LAM_SEED" text
    (given, _)           -> Right given
  pure (opts { randomSeed = seed }, rest)
  where
    go opts list = case list of
      "--deny-all" : rest -> go (allow [] opts) rest
      "--trace"    : rest -> go opts { tracing = True } rest
      "--dry-run"  : rest -> go opts { dryRunning = True } rest
      flag@('-' : '-' : _) : rest -> do
        changed <- readFlag opts flag
        go changed rest
      _ -> Right (opts, list)
    readFlag opts flag
      | Just n <- stripPrefix "--seed=" flag = do
          seed <- whole "--seed" n
          Right opts { randomSeed = seed }
      | Just new <- allowFlag flag = Right (allow [new] opts)
      | otherwise = Left ("lam: unknown flag: " <> T.pack flag)
    allow new opts = opts { permissions = Only (Map.unionWith wider
      (Map.fromList new) (case permissions opts of
                            AllowAll -> Map.empty
                            Only old -> old)) }
    -- Two flags for the same permission: the wider one, or both lists.
    -- 同じ権限のフラグが2つ：広いほうか、両方の一覧
    wider a b = case (a, b) of
      (Within xs, Within ys) -> Within (xs <> ys)
      _                      -> Everything
    -- 「種は整数で」
    whole name text = case readMaybe text of
      Just n  -> Right (Just n)
      Nothing -> Left ("lam: " <> name <> " needs a whole number: "
                         <> T.pack text)

-- 🚩 "--allow-read" or "--allow-read=data,logs".
-- "--allow-read" か "--allow-read=data,logs"
allowFlag :: String -> Maybe (Permission, Scope)
allowFlag flag = do
  rest <- T.stripPrefix "--allow-" (T.pack flag)
  let (name, value) = T.breakOn "=" rest
  permission <- lookup name
    [ ("net", Net), ("read", Read), ("write", Write), ("env", Env)
    , ("run", Run) ]
  pure $ if T.null value
    then (permission, Everything)
    else (permission, Within (filter (not . T.null)
                                (T.splitOn "," (T.drop 1 value))))

-- ANCHOR_END: options

-- 📖 How to use lam, for a flag it does not know.
-- 知らないフラグのときに見せる、lam の使い方
usage :: Text
usage = T.unlines
  [ "usage: lam [flags] [file args...]"
  , "  --allow-net[=hosts]      --allow-read[=folders]"
  , "  --allow-write[=folders]  --allow-env[=names]"
  , "  --allow-run[=commands]   --deny-all"
  , "  --trace  --dry-run  --seed=n" ]

-- ANCHOR: guarded
-- 🧅 The guards for the flags, put around a handler: the trace
--    outside, so that it shows even the requests that are not allowed,
--    and the dry run inside, so that it tells only what is allowed.
--    It is made in IO, for the notebook of the tracer.
-- フラグに合わせた見張りを、ハンドラーにかぶせる。trace がいちばん外
-- なので、許されない頼みごとも見える。dry-run はいちばん内側なので、
-- 許されたことだけを知らせる。trace のノートのために、IO で作る
guarded :: MonadIO m => Options -> IO (Handler m -> Handler m)
guarded opts = do
  tracer <- newTracer
  let outside = if tracing opts then tracer else id
      inside  = if dryRunning opts then dryRun else id
  pure (outside . allowOnly (permissions opts) . inside)
-- ANCHOR_END: guarded
-- ANCHOR_END: all
