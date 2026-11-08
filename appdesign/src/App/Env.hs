-- ANCHOR: all
-- | The application's monad: ReaderT over IO, with the environment
--   (the connection and the logger) inside.
-- アプリのモナド：IO の上の ReaderT。環境（接続とロガー）を中に持つ
module App.Env
  ( Env (..)
  , App (..)
  , runApp
  , logInfo
  , withContext
  , db
  ) where

-- ANCHOR: imports
import Control.Monad.Reader
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
-- ANCHOR_END: imports

-- ANCHOR: env
-- 🎒 Everything the whole application needs to reach.
-- アプリ全体が手を伸ばしたいもの、全部
data Env = Env
  { envConn :: Connection
  , envLog  :: Text -> IO ()
  }

-- 🏗️ The application's own monad: read the Env, and do IO.
-- アプリ専用のモナド：Env を読めて、IO もできる
newtype App a = App { unApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env
-- ANCHOR_END: env

-- ANCHOR: helpers
-- 📢 Write a log line, with whatever logger the Env has.
-- ログを1行。Env に入っているロガーで
logInfo :: Text -> App ()
logInfo line = do
  logLine <- asks envLog
  liftIO (logLine line)

-- 🏷️ Inside this part, every log line starts with a tag.
-- この部分の中だけ、ログの行の頭に札を付ける
withContext :: Text -> App a -> App a
withContext tag = local (\env -> env { envLog = tagged (envLog env) })
  where
    tagged logLine line = logLine ("[" <> tag <> "] " <> line)

-- 🗃️ Run something that needs the connection.
-- 接続が要ることをする
db :: (Connection -> IO a) -> App a
db action = do
  conn <- asks envConn
  liftIO (action conn)
-- ANCHOR_END: helpers
-- ANCHOR_END: all
