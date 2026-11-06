-- ANCHOR: all
-- | The quest board as a queue: the front desk pins quests, the workers
--   take them, and at closing time everyone goes home.
-- キューで作る依頼の掲示板：受付が貼り、受付係が取り、閉店で全員が帰る
module Board
  ( Guild (..)
  , newGuild
  , takeOrClose
  , takeOrCloseShort
  , clerk
  , guildDay
  , nextQuest
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, forConcurrently_)
import Control.Concurrent.STM
import Data.Text (Text)

-- ANCHOR: guild
-- 🏪 The guild: a board with room for 3, the closing sign, and a log.
-- ギルド：3枚まで貼れる掲示板、閉店の札、こなした依頼の記録
data Guild = Guild
  { board  :: TBQueue Text
  , closed :: TVar Bool
  , done   :: TVar [(Int, Text)]
  }

newGuild :: IO Guild
newGuild = Guild <$> newTBQueueIO 3 <*> newTVarIO False <*> newTVarIO []
-- ANCHOR_END: guild

-- ANCHOR: clerk
-- 📥 The next quest; or Nothing, when the board is empty AND we closed.
-- 次の依頼。掲示板が空で、しかも閉店していたら Nothing
takeOrClose :: Guild -> STM (Maybe Text)
takeOrClose guild =
  (Just <$> readTBQueue (board guild))
    `orElse` (do
      isClosed <- readTVar (closed guild)
      check isClosed
      pure Nothing)

-- 🧑‍💼 A clerk: take quests and do them, until closing time.
-- 受付係：依頼を取ってはこなす。閉店まで
clerk :: Guild -> Int -> IO ()
clerk guild me = loop
  where
    loop = do
      next <- atomically (takeOrClose guild)
      case next of
        Nothing    -> pure ()
        Just quest -> do
          threadDelay 10000
          atomically (modifyTVar' (done guild) ((me, quest) :))
          loop
-- ANCHOR_END: clerk

-- ANCHOR: day
-- ☀️ A day at the guild: pin all the quests, close, and wait for
--    the clerks to finish.
-- ギルドの1日：依頼を全部貼り、閉店して、受付係が終えるのを待つ
guildDay :: Int -> [Text] -> IO [(Int, Text)]
guildDay clerks quests = do
  guild <- newGuild
  concurrently_
    (forConcurrently_ [1 .. clerks] (clerk guild))
    (do
      mapM_ (atomically . writeTBQueue (board guild)) quests
      atomically (writeTVar (closed guild) True))
  readTVarIO (done guild)
-- ANCHOR_END: day

-- ANCHOR: urgent
-- 🚨 An urgent quest if there is one, or else a normal one.
-- 急ぎの依頼があればそれを、なければふつうの依頼を
nextQuest :: TQueue Text -> TQueue Text -> STM Text
nextQuest urgent normal = readTQueue urgent `orElse` readTQueue normal
-- ANCHOR_END: urgent

-- ANCHOR: short
-- 📥 takeOrClose, written with <|> (orElse) and <$ (replace the answer).
-- takeOrClose を、<|>（orElse）と <$（答えを入れかえる）で
takeOrCloseShort :: Guild -> STM (Maybe Text)
takeOrCloseShort guild =
      Just <$> readTBQueue (board guild)
  <|> Nothing <$ (check =<< readTVar (closed guild))
-- ANCHOR_END: short
-- ANCHOR_END: all
