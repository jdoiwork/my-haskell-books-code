-- ANCHOR: all
-- | The front desk as an application: the boxes live in the Env, and
--   the rest of the program only sees what the desk can do.
-- アプリとしての受付：箱は Env の中。外からは、受付ができることだけが見える
-- ANCHOR: exports
module Guild
  ( Env
  , newEnv
  , App
  , runApp
  , nextTicket
  , postQuest
  , boardSize
  , say
  , welcome
  , welcomeAll
  ) where
-- ANCHOR_END: exports

-- ANCHOR: imports
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- The unliftio versions: these work in any monad that can run IO
-- inside (like App), not only in IO.
-- unliftio の版：IO だけでなく、中で IO を動かせるモナド（App など）で使える
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (forConcurrently)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef)
import UnliftIO.MVar (MVar, newMVar, withMVar)
import UnliftIO.STM
  (TBQueue, atomically, lengthTBQueue, newTBQueueIO, writeTBQueue)
-- ANCHOR_END: imports

-- ANCHOR: env
-- 🎒 The boxes of the front desk. The fields are not exported.
-- 受付の箱たち。フィールドは外に見せない
data Env = Env
  { envTickets :: IORef Int
  , envBoard   :: TBQueue Text
  , envLogLock :: MVar ()
  }

newEnv :: IO Env
newEnv = Env <$> newIORef 1 <*> newTBQueueIO 10 <*> newMVar ()

-- 🏗️ The application's monad (ReaderT pattern).
-- アプリのモナド（ReaderT パターン）
newtype App a = App { unApp :: ReaderT Env IO a }
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env
-- ANCHOR_END: env

-- ANCHOR: desk
-- 🎟️ Hand out the next ticket number.
-- 次の番号札を渡す
nextTicket :: App Int
nextTicket = do
  box <- asks envTickets
  atomicModifyIORef' box (\n -> (n + 1, n))

-- 📌 Pin a quest on the board (wait if the board is full).
-- 依頼を掲示板に貼る（いっぱいなら待つ）
postQuest :: Text -> App ()
postQuest quest = do
  queue <- asks envBoard
  atomically (writeTBQueue queue quest)

-- 🔢 How many quests are on the board?
-- 掲示板に、いま何枚？
boardSize :: App Int
boardSize = do
  queue <- asks envBoard
  fromIntegral <$> atomically (lengthTBQueue queue)

-- 📢 Say some lines together, without anyone cutting in.
-- 何行かを、だれにも割り込まれずに続けて言う
say :: [Text] -> App ()
say messages = do
  lock <- asks envLogLock
  withMVar lock $ \_ ->
    mapM_ (liftIO . TIO.putStrLn) messages
-- ANCHOR_END: desk

-- ANCHOR: welcome
-- 👋 Welcome a visitor: a ticket, and a greeting.
-- 訪問者を迎える：番号札と、あいさつ
welcome :: Text -> App Int
welcome name = do
  n <- nextTicket
  say
    -- 「ようこそ、○○さん！」
    [ "Welcome, " <> name <> "!"
    -- 「あなたの番号は ○ です」
    , "  Your number is " <> T.pack (show n) <> "."
    ]
  pure n

-- 👥 Welcome everyone at once, from inside App.
-- 全員を同時に迎える。App の中から
welcomeAll :: [Text] -> App [Int]
welcomeAll names = forConcurrently names welcome
-- ANCHOR_END: welcome
-- ANCHOR_END: all
