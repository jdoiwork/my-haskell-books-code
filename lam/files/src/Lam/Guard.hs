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
  , covers
  , allowOnly
  , traced
  , dryRun
  , traceLine
  , Options (..)
  , defaultOptions
  , readOptions
  , guarded
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as H
import System.FilePath (normalise, splitDirectories)

import Lam.Builtins (con)
import Lam.Eval
import Lam.Files (globRoot)
import Lam.Io (Handler)
import Lam.Pretty (renderString)
-- ANCHOR_END: head

-- ANCHOR: permission
-- 🔑 What a request may need to be allowed.
-- 頼みごとが、許してもらう必要のあるもの
data Permission = Net | Read | Write | Env | Run
  deriving (Eq, Ord, Show, Enum, Bounded)

-- 🔑 What one permission covers: everything, or only these hosts,
--    folders, or names.
-- 1つの権限が及ぶ範囲：全部か、これらのホスト・フォルダー・名前だけか
data Scope = Everything | Within [Text]
  deriving (Eq, Show)

-- 🔑 Everything (no flags), or only the permissions in the Map.
-- 全部（フラグなし）か、Map にある権限だけか
data Permissions = AllowAll | Only (Map Permission Scope)
  deriving (Eq, Show)

-- 🔑 The permissions a request needs, each with what it is about:
--    the host of a URL, the path of a file, the name of a variable.
-- 頼みごとに要る権限と、その相手：URL のホスト、ファイルの道、
-- 変数の名前
needs :: Request -> [(Permission, Text)]
needs request = case request of
  ReadFile path        -> [(Read, path)]
  WriteFile path _     -> [(Write, path)]
  HttpSend _ url _ _   -> [(Net, hostOf url)]
  SqlOpen ":memory:" _ -> []
  SqlOpen file ReadOnly  -> [(Read, file)]
  SqlOpen file ReadWrite -> [(Read, file), (Write, file)]
  LookupEnv name       -> [(Env, name)]
  -- ANCHOR: needsFiles
  -- Looking needs --allow-read; changing needs --allow-write (both the
  -- old and the new name, for rename). A glob reads the folder it
  -- starts from.
  -- 見るには --allow-read、変えるには --allow-write（rename は、前と後の
  -- 両方の名前）。glob は、探し始めるフォルダーを読む
  ListDir dir          -> [(Read, dir)]
  Glob pattern         -> [(Read, globRoot pattern)]
  Exists path          -> [(Read, path)]
  IsDir path           -> [(Read, path)]
  FileInfo path        -> [(Read, path)]
  MakeDir dir          -> [(Write, dir)]
  Remove path          -> [(Write, path)]
  Rename from to       -> [(Write, from), (Write, to)]
  CopyFile from to     -> [(Read, from), (Write, to)]
  AppendFile path _    -> [(Write, path)]
  -- A command needs --allow-run, with its name.
  -- コマンドには --allow-run が要る。相手はその名前
  RunProcess command _ _ -> [(Run, command)]
  -- ANCHOR_END: needsFiles
  _                    -> []

-- 🌐 The host of a URL, read by http-client itself, so that it is the
--    host the request will really go to. Not a URL: the text itself.
-- URL のホスト。http-client 自身に読ませるので、本当に行く先になる。
-- URL でなければ、その文字のまま
hostOf :: Text -> Text
hostOf url =
  maybe url (TE.decodeUtf8 . H.host) (H.parseRequest (T.unpack url))
-- ANCHOR_END: permission

-- ANCHOR: covers
-- 🎯 Does a scope cover this host, path, or name?
--    A folder covers the paths inside it, piece by piece: "data" covers
--    "data/a.txt" but not "data2/a.txt". A path with ".." is covered
--    only by Everything: "data/../secret.txt" goes out of data.
-- 範囲が、このホスト・道・名前に及ぶか。フォルダーは、その中の道に
-- 及ぶ。くらべるのは区切りごと："data" は "data/a.txt" に及ぶが、
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
--    Err (the requests that need a permission to touch a file or the
--    network all answer with a Result), or Nothing for lookupEnv.
--    getEnvironment is asked, and only the names allowed are kept.
-- 要る権限が全部あり、相手に及ぶときだけ、ハンドラーに頼む。なければ
-- 頼まずに答える：Err（ファイルやネットに触れる頼みごとは、どれも
-- Result で答える）か、lookupEnv なら Nothing。getEnvironment は頼んで、
-- 許した名前だけを残す
allowOnly :: Monad m => Permissions -> Handler m -> Handler m
allowOnly AllowAll handle = handle
allowOnly (Only given) handle = \request -> case request of
  GetEnvironment -> do
    answer <- handle request
    pure $ case answer of
      VMap env -> VMap (Map.filterWithKey (\k _ -> allowed Env k) env)
      other    -> other
  _ -> case filter (not . uncurry allowed) (needs request) of
    [] -> handle request
    (missing, what) : _ -> pure $ case request of
      LookupEnv _ -> con "Nothing" []
      -- ANCHOR: deniedFiles
      -- Not allowed to look: as if it were not there.
      -- 見るのを許されていない：ないのと同じ
      Exists _ -> VBool False
      IsDir _  -> VBool False
      -- "git: needs --allow-run=git": the command does not start.
      -- コマンドは始まらない
      RunProcess command _ _ -> con "CannotStart"
        [VString (command <> ": needs " <> flag missing <> "=" <> what)]
      -- ANCHOR_END: deniedFiles
      -- "https://pokeapi.co/…: needs --allow-net=pokeapi.co": the flag
      -- to add, as it is. / 足すフラグを、そのままの形で
      HttpSend _ url _ _ ->
        con "Err" [VString (url <> ": needs " <> flag missing <> "=" <> what)]
      _ -> con "Err"
        [VString (what <> ": needs " <> flag missing <> "=" <> what)]
  where
    allowed p what = maybe False (\scope -> covers p scope what)
                       (Map.lookup p given)
    flag p = case p of
      Net   -> "--allow-net"
      Read  -> "--allow-read"
      Write -> "--allow-write"
      Env   -> "--allow-env"
      Run   -> "--allow-run"
-- ANCHOR_END: allowOnly

-- ANCHOR: traced
-- 👣 Write each request to the standard error before doing it. It uses
--    the same handler (PutErr), so a fake world keeps the trace, too.
-- 頼みごとを、やる前に標準エラーに書く。同じハンドラー（PutErr）で書く
-- ので、偽物の世界にも記録が残る
traced :: Monad m => Handler m -> Handler m
traced handle request = do
  case traceLine request of
    Just line -> () <$ handle (PutErr ("trace: " <> line <> "\n"))
    Nothing   -> pure ()
  handle request

-- 📝 A request in one line, as a Lam call. Writing to the screen is
--    not shown: it is on the screen already.
-- 頼みごとを、Lam の呼び出しの形で1行に。画面に書くのは見せない
-- （もう画面に出ている）
traceLine :: Request -> Maybe Text
traceLine request = case request of
  PutStr _          -> Nothing
  PutErr _          -> Nothing
  GetLine           -> Just "getLine"
  ReadFile p        -> Just ("readFile " <> quote p)
  WriteFile p t     -> Just ("writeFile " <> quote p <> " ("
                               <> T.pack (show (T.length t)) <> " letters)")
  GetArgs           -> Just "getArgs"
  LookupEnv k       -> Just ("lookupEnv " <> quote k)
  GetEnvironment    -> Just "getEnvironment"
  GetContents       -> Just "getContents"
  HttpSend m u _ _  -> Just ("Http.send " <> quote m <> " " <> quote u)
  SqlOpen f ReadWrite -> Just ("Sql.open " <> quote f)
  SqlOpen f ReadOnly  -> Just ("Sql.openReadOnly " <> quote f)
  SqlQuery n s cs   -> Just ("Sql.query #" <> number n <> " " <> quote s
                               <> cells cs)
  SqlExec n s cs    -> Just ("Sql.exec #" <> number n <> " " <> quote s
                               <> cells cs)
  SqlClose n        -> Just ("Sql.close #" <> number n)
  -- ANCHOR: traceFiles
  ListDir d         -> Just ("File.list " <> quote d)
  Glob p            -> Just ("File.glob " <> quote p)
  Exists p          -> Just ("File.exists " <> quote p)
  IsDir p           -> Just ("File.isDir " <> quote p)
  FileInfo p        -> Just ("File.info " <> quote p)
  MakeDir d         -> Just ("File.makeDir " <> quote d)
  Remove p          -> Just ("File.remove " <> quote p)
  Rename a b        -> Just ("File.rename " <> quote a <> " " <> quote b)
  CopyFile a b      -> Just ("File.copy " <> quote a <> " " <> quote b)
  AppendFile p t    -> Just ("File.append " <> quote p <> " ("
                               <> T.pack (show (T.length t)) <> " letters)")
  RunProcess c as i -> Just (commandLine c as i)
  Now               -> Just "Time.now"
  Here              -> Just "Time.here"
  RandomInt a b     -> Just ("Random.int " <> number a <> " " <> number b)
  -- ANCHOR_END: traceFiles
  where
    quote = renderString             -- "ピカチュウ", as Lam writes it
    number :: Show n => n -> Text
    number = T.pack . show
    cells cs = " [" <> T.intercalate ", " (map cell cs) <> "]"
    cell c = case c of
      CNull    -> "JNull"
      CInt i   -> T.pack (show i)
      CDec n d -> "JDec " <> T.pack (show n) <> " " <> T.pack (show d)
      CText s  -> quote s
-- ANCHOR_END: traced

-- ANCHOR: dryRun
-- 🧪 Do what only reads, and only tell what would write: writing a
--    file, an HTTP request that is not GET, and Sql.exec. A database is
--    opened only to read, so even a mistake cannot write to it.
-- 読むだけのことはやり、書くことは知らせるだけにする：ファイルに書く、
-- GET でない HTTP、Sql.exec。データベースは読むだけに開くので、
-- まちがえても書けない
dryRun :: Monad m => Handler m -> Handler m
dryRun handle request = case request of
  WriteFile p t -> do
    tell ("writeFile " <> renderString p <> " ("
            <> T.pack (show (T.length t)) <> " letters)")
    pure (con "Ok" [VTuple []])
  HttpSend m u _ _ | m `notElem` ["GET", "HEAD"] -> do
    tell (m <> " " <> u)
    -- 「試しに動かしているので、送っていない」
    pure (con "Err"
      [VString ("dry run: " <> m <> " " <> u <> " was not sent")])
  SqlOpen f ReadWrite -> handle (SqlOpen f ReadOnly)
  SqlExec _ s _ -> do
    tell ("Sql.exec " <> renderString s)
    pure (con "Ok" [VInt 0])
  -- ANCHOR: dryRunFiles
  -- Changing files and folders: only tell, and answer Ok.
  -- ファイルとフォルダーを変えること：知らせるだけで、Ok と答える
  MakeDir d      -> told ("File.makeDir " <> renderString d)
  Remove p       -> told ("File.remove " <> renderString p)
  Rename a b     -> told ("File.rename " <> renderString a <> " "
                            <> renderString b)
  CopyFile a b   -> told ("File.copy " <> renderString a <> " "
                            <> renderString b)
  AppendFile p t -> told ("File.append " <> renderString p <> " ("
                            <> T.pack (show (T.length t)) <> " letters)")
  -- A command may change anything, so it is not run at all.
  -- コマンドは何を変えるかわからないので、まったく動かさない
  RunProcess c as i -> do
    tell (commandLine c as i)
    pure (con "CannotStart" [VString ("dry run: " <> c <> " was not run")])
  -- ANCHOR_END: dryRunFiles
  _ -> handle request
  where
    tell line = () <$ handle (PutErr ("dry-run: " <> line <> "\n"))
    told line = tell line >> pure (con "Ok" [VTuple []])

-- 📝 Process.run "git" ["log"] (or Process.pipe, with the length of
--    the input).
-- Process.run の形で1行に（入力があれば Process.pipe と長さ）
commandLine :: Text -> [Text] -> Text -> Text
commandLine c as i
  | T.null i  = "Process.run " <> renderString c <> " " <> args
  | otherwise = "Process.pipe " <> renderString c <> " " <> args <> " ("
                  <> T.pack (show (T.length i)) <> " letters)"
  where
    args = "[" <> T.intercalate ", " (map renderString as) <> "]"
-- ANCHOR_END: dryRun

-- ANCHOR: options
-- 🚩 The flags before the file name.
-- ファイルの名前より前のフラグ
data Options = Options
  { permissions :: Permissions
  , tracing     :: Bool
  , dryRunning  :: Bool
  -- ANCHOR: seedField
  , randomSeed  :: Maybe Int     -- --seed=618: the same dice every time
  -- ANCHOR_END: seedField
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options AllowAll False False Nothing

-- 🚩 Read the flags at the head of the arguments; the rest is the file
--    and its arguments. No flag: everything is allowed. --allow-...
--    or --deny-all: only what is allowed. --allow-net=a,b gives only
--    those hosts (and folders for read and write, names for env).
-- 引数の頭のフラグを読む。残りは、ファイルとその引数。フラグなし：
-- 全部許す。--allow-… か --deny-all：許したものだけ。--allow-net=a,b
-- なら、そのホストだけ（read と write はフォルダー、env は名前）
readOptions :: [String] -> Either String (Options, [String])
readOptions = go defaultOptions
  where
    go opts args = case args of
      "--deny-all"    : rest -> go (allow [] opts) rest
      "--trace"       : rest -> go opts { tracing = True } rest
      "--dry-run"     : rest -> go opts { dryRunning = True } rest
      -- ANCHOR: seedFlag
      flag@('-' : '-' : 's' : 'e' : 'e' : 'd' : '=' : n) : rest ->
        case reads n of
          [(seed, "")] -> go opts { randomSeed = Just seed } rest
          _            -> Left ("not a number: " <> flag)
      -- ANCHOR_END: seedFlag
      flag@('-' : '-' : _) : rest -> case allowFlag flag of
        Just new -> go (allow [new] opts) rest
        Nothing  -> Left ("unknown flag: " <> flag)
      _ -> Right (opts, args)
    allow new opts = opts { permissions = Only (Map.unionWith wider
      (Map.fromList new) (case permissions opts of
                            AllowAll -> Map.empty
                            Only old -> old)) }
    -- Two flags for the same permission: the wider one, or both lists.
    -- 同じ権限のフラグが2つ：広いほうか、両方の一覧
    wider a b = case (a, b) of
      (Within xs, Within ys) -> Within (xs <> ys)
      _                      -> Everything

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

-- 🧅 Put the guards around a handler: the trace outside, so that it
--    shows even the requests that are not allowed.
-- ハンドラーに見張りをかぶせる。trace がいちばん外なので、許されない
-- 頼みごとも見える
guarded :: Monad m => Options -> Handler m -> Handler m
guarded opts handle =
  (if tracing opts then traced else id)
    (allowOnly (permissions opts)
      ((if dryRunning opts then dryRun else id) handle))
-- ANCHOR_END: options
-- ANCHOR_END: all
