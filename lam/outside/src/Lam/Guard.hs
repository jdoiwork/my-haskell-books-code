-- ANCHOR: all
-- ANCHOR: head
{-# LANGUAGE OverloadedStrings #-}
-- | Guards around a handler: permissions (--allow-net, ...), --trace
--   and --dry-run. Each one takes a handler and gives a handler, so
--   they work the same in the real world and in a fake one.
-- ハンドラーをくるむ見張り：権限（--allow-net など）、--trace、
-- --dry-run。どれも、ハンドラーを受け取ってハンドラーを返すので、
-- 本物の世界でも偽物の世界でも、同じように効く。
module Lam.Guard
  ( Permission (..)
  , Permissions (..)
  , needs
  , allowOnly
  , traced
  , dryRun
  , traceLine
  , Options (..)
  , defaultOptions
  , readOptions
  , guarded
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Lam.Builtins (con)
import Lam.Eval
import Lam.Io (Handler)
import Lam.Pretty (renderString)
-- ANCHOR_END: head

-- ANCHOR: permission
-- 🔑 What a request may need to be allowed.
-- 頼みごとが、許してもらう必要のあるもの
data Permission = Net | Read | Write
  deriving (Eq, Ord, Show, Enum, Bounded)

-- 🔑 Everything (no flags), or only these.
-- 全部（フラグなし）か、これだけか
data Permissions = AllowAll | Only (Set Permission)
  deriving (Eq, Show)

-- 🔑 The permissions a request needs. Reading the arguments, the
--    environment and the keyboard needs none.
-- 頼みごとに要る権限。引数・環境変数・キーボードを読むのには要らない
needs :: Request -> [Permission]
needs request = case request of
  ReadFile _          -> [Read]
  WriteFile _ _       -> [Write]
  HttpSend {}         -> [Net]
  SqlOpen ":memory:" _ -> []
  SqlOpen _ ReadOnly  -> [Read]
  SqlOpen _ ReadWrite -> [Read, Write]
  _                   -> []
-- ANCHOR_END: permission

-- ANCHOR: allowOnly
-- 🚧 Ask the handler only when every permission the request needs is
--    given. Otherwise answer Err without asking: the requests that need
--    a permission all answer with a Result, so a script can go on.
-- 要る権限が全部あるときだけ、ハンドラーに頼む。なければ、頼まずに
-- Err で答える。権限の要る頼みごとは、どれも Result で答えるので、
-- スクリプトは続けられる
allowOnly :: Applicative m => Permissions -> Handler m -> Handler m
allowOnly AllowAll handle = handle
allowOnly (Only given) handle = \request ->
  case filter (`Set.notMember` given) (needs request) of
    []          -> handle request
    missing : _ -> pure (con "Err" [VString (target request <> ": needs "
                                               <> flag missing)])
  where
    flag p = case p of
      Net   -> "--allow-net"
      Read  -> "--allow-read"
      Write -> "--allow-write"
-- ANCHOR_END: allowOnly

-- 🎯 What a request is about: a file, a URL, a database.
-- 頼みごとの相手：ファイル、URL、データベース
target :: Request -> Text
target request = case request of
  ReadFile path     -> path
  WriteFile path _  -> path
  HttpSend _ url _ _ -> url
  SqlOpen file _    -> file
  _                 -> "this request"

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
  where
    quote = renderString             -- "ピカチュウ", as Lam writes it
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
  _ -> handle request
  where
    tell line = () <$ handle (PutErr ("dry-run: " <> line <> "\n"))
-- ANCHOR_END: dryRun

-- ANCHOR: options
-- 🚩 The flags before the file name.
-- ファイルの名前より前のフラグ
data Options = Options
  { permissions :: Permissions
  , tracing     :: Bool
  , dryRunning  :: Bool
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options AllowAll False False

-- 🚩 Read the flags at the head of the arguments; the rest is the file
--    and its arguments. No flag: everything is allowed. --allow-...
--    or --deny-all: only what is allowed.
-- 引数の頭のフラグを読む。残りは、ファイルとその引数。フラグなし：
-- 全部許す。--allow-… か --deny-all：許したものだけ
readOptions :: [String] -> Either String (Options, [String])
readOptions = go defaultOptions
  where
    go opts args = case args of
      "--allow-net"   : rest -> go (allow Net opts) rest
      "--allow-read"  : rest -> go (allow Read opts) rest
      "--allow-write" : rest -> go (allow Write opts) rest
      "--deny-all"    : rest -> go (only Set.empty opts) rest
      "--trace"       : rest -> go opts { tracing = True } rest
      "--dry-run"     : rest -> go opts { dryRunning = True } rest
      ('-' : '-' : flag) : _ -> Left ("unknown flag: --" <> flag)
      _                      -> Right (opts, args)
    allow p opts = only (Set.singleton p) opts
    only ps opts = opts { permissions = case permissions opts of
      AllowAll -> Only ps
      Only old -> Only (Set.union old ps) }

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
