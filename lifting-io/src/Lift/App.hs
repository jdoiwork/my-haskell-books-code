-- ANCHOR: all
-- | The guild's App, as in the chapter on application design:
--   ReaderT over IO, with the environment inside.
-- ギルドの App。アプリケーション設計の章と同じ：IO の上の ReaderT。
-- 環境を中に持つ
module Lift.App
  ( Env (..)
  , guildEnv
  , App (..)
  , runApp
  , logInfo
  , appraise
  ) where

-- ANCHOR: imports
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import UnliftIO (MonadUnliftIO (..))
-- ANCHOR_END: imports

-- ANCHOR: env
-- 🎒 What the whole guild needs to reach.
-- ギルド全体が手を伸ばしたいもの
data Env = Env
  { envRate :: Int             -- gold per letter / 1文字あたりの金貨
  , envLog  :: Text -> IO ()   -- the logger / ロガー
  }

-- 🏰 The usual environment: write the log to the screen.
-- いつもの環境：ログは画面に書く
guildEnv :: Env
guildEnv = Env
  { envRate = 100
  , envLog  = \line -> TIO.putStrLn ("[log] " <> line)
  }

-- 🏗️ The application's own monad: read the Env, and do IO.
-- アプリ専用のモナド：Env を読めて、IO もできる
newtype App a = App { unApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env

-- 📢 Write a log line, with whatever logger the Env has.
-- ログを1行。Env に入っているロガーで
logInfo :: Text -> App ()
logInfo line = do
  write <- asks envLog
  liftIO (write line)
-- ANCHOR_END: env

-- ANCHOR: appraise
-- 🔍 Appraise an item: it takes 0.1 seconds, and the price depends on
--    the guild's rate.
-- 品物を鑑定する：0.1 秒かかり、値段はギルドの相場で決まる
appraise :: Text -> App Int
appraise item = do
  rate <- asks envRate
  liftIO (threadDelay 100000)
  pure (T.length item * rate)
-- ANCHOR_END: appraise

-- ANCHOR: unlift
-- 🔓 App can give back "a way to run App as IO": just pass the Env again.
-- App は「App を IO として動かす方法」を渡せる。Env を渡し直すだけ
instance MonadUnliftIO App where
  withRunInIO inner = App (ReaderT (\env -> inner (runApp env)))
-- ANCHOR_END: unlift
-- ANCHOR_END: all
