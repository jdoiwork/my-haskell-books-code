-- ANCHOR: all
-- ANCHOR: header
-- | What the shelf needs when it is handed to others in a container:
--   settings from the environment (and a password from a file, as Docker
--   secrets give it), and patience while the database is starting.
-- 本棚をコンテナで人に渡すときに要るもの：環境変数からの設定（パスワードは
-- ファイルから。Docker の secrets はファイルで渡す）と、データベースが
-- 起きるのを待つ辛抱強さ
module Tsundoku.Deploy
  ( Settings (..)
  , readSettings
  , storeSetting
  , retryOpen
  , webSite
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException,
                          fromException, throwIO, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- the site: the API, and the folder of the React screen
-- サイト：API と、React の画面のフォルダー
import Network.Wai (Application, pathInfo)
import Servant (Proxy (..), Raw, serve, serveDirectoryWebApp)

import Tsundoku.Api.Server (Clock, shelfApp)
import Tsundoku.Shelf (Shelf)
-- ANCHOR_END: header

-- ANCHOR: settings
-- ⚙️ Where to keep the shelf, and which address to wait on.
-- 本棚をしまう場所と、待つ住所と、配る画面
data Settings = Settings
  { store :: String     -- "sqlite:…", "postgres:…", or "memory"
  , host  :: String     -- 127.0.0.1 on your machine, 0.0.0.0 in a container
  , web   :: Maybe FilePath  -- the React screen, built (none: htmx)
  }
  deriving (Show, Eq)

-- 📖 Read the settings from the environment:
--      TSUNDOKU_DB               the store (sqlite:tsundoku.db if unset)
--      TSUNDOKU_DB_PASSWORD_FILE a file holding PostgreSQL's password
--      TSUNDOKU_HOST             the address (127.0.0.1 if unset)
--      TSUNDOKU_WEB              the folder of the React screen, built
--                                (the htmx pages if unset)
-- 環境変数から設定を読む：
--      TSUNDOKU_DB               しまう場所（なければ sqlite:tsundoku.db）
--      TSUNDOKU_DB_PASSWORD_FILE PostgreSQL のパスワードを書いたファイル
--      TSUNDOKU_HOST             住所（なければ 127.0.0.1）
--      TSUNDOKU_WEB              ビルドした React の画面のフォルダー
--                                （なければ htmx のページ）
readSettings :: IO Settings
readSettings = do
  setting  <- lookupEnv "TSUNDOKU_DB"
  file     <- lookupEnv "TSUNDOKU_DB_PASSWORD_FILE"
  password <- traverse readFile file
  address  <- lookupEnv "TSUNDOKU_HOST"
  folder   <- lookupEnv "TSUNDOKU_WEB"
  pure Settings
    { store = storeSetting setting password
    , host  = fromMaybe "127.0.0.1" address
    , web   = folder
    }

-- 🔑 The store, with the password added for PostgreSQL. The setting is in
--    libpq's keyword form ("host=db user=shelf dbname=tsundoku"), so the
--    password goes on the end, in single quotes ('\' and ''' escaped).
-- しまう場所。PostgreSQL ならパスワードを足す。設定は libpq のキーワードの
-- 形（"host=db user=shelf dbname=tsundoku"）なので、パスワードは最後に、
-- 一重引用符でくるんで足す（\ と ' はエスケープする）
storeSetting :: Maybe String -> Maybe String -> String
storeSetting setting password = case (stripPrefix "postgres:" chosen, password) of
  (Just conn, Just pw) -> "postgres:" <> conn <> " password=" <> quoted (trim pw)
  _                    -> chosen
  where
    chosen   = fromMaybe "sqlite:tsundoku.db" setting
    -- only the line break at the end (as the PostgreSQL image does);
    -- a space is part of the password
    -- 落とすのは最後の改行だけ（PostgreSQL のイメージと同じ）。空白は
    -- パスワードの一部
    trim     = dropWhileEnd (`elem` ("\r\n" :: String))
    quoted s = "'" <> concatMap escape s <> "'"
    escape c = if c `elem` ['\\', '\''] then ['\\', c] else [c]
-- ANCHOR_END: settings

-- ANCHOR: retry
-- 🔁 Open the shelf, trying again if opening fails (the database may
--    still be starting). Once the shelf is open and the work has begun,
--    a failure is not retried: that is the work's own failure. Ctrl-C
--    is not retried either: it asks us to stop, not to try again.
-- 本棚を開く。開けなかったら、やり直す（データベースが、まだ起きている
-- ところかもしれない）。本棚が開いて仕事が始まったあとの失敗は、
-- やり直さない（それは仕事そのものの失敗だから）。Ctrl-C も、やり直さ
-- ない（「やめて」の合図で、「もう一度」ではないから）
retryOpen
  :: Int -> Int -> ((shelf -> IO a) -> IO a) -> (shelf -> IO a) -> IO a
retryOpen tries pause open work = go tries
  where
    go n = do
      begun <- newIORef False
      let started s = writeIORef begun True >> work s
      -- try, not catch: the next try runs outside the handler, so it is
      --   not masked (the exceptions chapter: do not retry in a handler)
      -- catch ではなく try。次の試しは、受け止め方の外で、mask の外で動く
      -- （IO の例外の章：受け止め方の中でやり直さない）
      result <- try (open started)
      case result of
        Right a -> pure a
        Left e  -> do
          didBegin <- readIORef begun
          if didBegin || n <= 1 || isStop e
            then throwIO e
            else do
              hPutStrLn stderr
                ("cannot open the shelf yet; trying again: " <> show e)
              threadDelay pause
              go (n - 1)

-- 🛑 An asynchronous exception: Ctrl-C (UserInterrupt), a thread killed,
--    a timeout. Someone outside asks us to stop.
-- 非同期例外：Ctrl-C（UserInterrupt）、止められたスレッド、時間切れ。
-- 外のだれかが「やめて」と言っている
isStop :: SomeException -> Bool
isStop e = isJust (fromException e :: Maybe SomeAsyncException)
-- ANCHOR_END: retry

-- ANCHOR: webSite
-- 🌐 The site handed out: the JSON API under /api, and the React screen
--    (what Vite built into a folder) everywhere else; "/" is its
--    index.html. The screen asks /api on the same server, so no proxy
--    is needed.
-- 配るサイト：/api の下は JSON の API、ほかは全部 React の画面（Vite が
-- フォルダーに作ったもの）。「/」は index.html。画面は同じサーバーの /api
-- に聞くので、中継は要らない
webSite :: FilePath -> Clock -> Shelf -> Application
webSite folder clock shelf request reply =
  case pathInfo request of
    "api" : rest -> shelfApp clock shelf request { pathInfo = rest } reply
    []           -> files request { pathInfo = ["index.html"] } reply
    _            -> files request reply
  where
    files = serve (Proxy :: Proxy Raw) (serveDirectoryWebApp folder)
-- ANCHOR_END: webSite
-- ANCHOR_END: all
