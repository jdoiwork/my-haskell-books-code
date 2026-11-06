-- ANCHOR: all
-- | A small supervisor, after Erlang/OTP: children that fall over are
--   started again, by a rule written as a pure function.
-- Erlang/OTP にならった、小さな監督役：倒れた子を、また始める。
-- その決まりは、純粋な関数で書く
module MiniMq.Supervisor
  ( Child (..)
  , Strategy (..)
  , Intensity (..)
  , Plan (..)
  , Decision (..)
  , decide
  , TooManyRestarts (..)
  , supervise
  ) where

import Control.Concurrent.Async (async, cancel, waitCatchSTM)
import Control.Concurrent.STM (atomically)
import Control.Exception (Exception, finally, throwIO)
import Data.Foldable (asum, for_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)

-- ANCHOR: types
-- 👶 A child: a name, and work that should go on forever.
-- 子：名前と、ずっと続くはずの仕事
data Child = Child
  { childName :: Text
  , work      :: IO ()
  }

-- 🧭 Who starts again when one child falls.
-- 1人が倒れたとき、誰が始め直すか
data Strategy
  = OneForOne    -- ^ only the one that fell / 倒れた子だけ
  | OneForAll    -- ^ everyone / 全員
  | RestForOne   -- ^ the one that fell, and those started after it
                 --   倒れた子と、そのあとに始めた子たち
  deriving (Show, Eq)

-- 📏 At most `restarts` restarts within `seconds`. One more, and the
--    supervisor gives up and falls over itself.
-- seconds 秒のうちに、restarts 回まで始め直す。それを超えたら、
-- 監督役はあきらめて、自分が倒れる
data Intensity = Intensity
  { restarts :: Int
  , seconds  :: Double
  }
  deriving (Show, Eq)

-- 📋 The whole plan: how to restart, how often, and whom.
-- 計画の全部：どう始め直すか、何回まで、誰を
data Plan = Plan
  { strategy  :: Strategy
  , intensity :: Intensity
  , children  :: [Child]
  }
-- ANCHOR_END: types

-- ANCHOR: decide
-- ⚖️ The rule, as a pure function. Given the time now, the names of the
--    children (in the order they started), who fell, and the times of
--    the recent restarts (newest first), say what to do.
-- 決まりを、純粋な関数で。今の時刻、子の名前（始めた順）、倒れた子、
-- 最近の始め直しの時刻（新しい順）から、どうするかを決める
data Decision = Restart [Text] | GiveUp
  deriving (Show, Eq)

decide :: Strategy -> Intensity -> Double -> [Text] -> Text -> [Double]
       -> (Decision, [Double])
decide how limit now names fallen history
  | length recent >= restarts limit = (GiveUp, recent)
  | otherwise                       = (Restart again, now : recent)
  where
    recent = takeWhile (> now - seconds limit) history
    again  = case how of
      OneForOne  -> [fallen]
      OneForAll  -> names
      RestForOne -> dropWhile (/= fallen) names
-- ANCHOR_END: decide

-- ANCHOR: supervise
-- 🏳️ What a supervisor throws when it gives up. Its own supervisor
--    (if any) sees it fall, like any other child.
-- 監督役があきらめたときに投げるもの。その上の監督役（がいれば）には、
-- ほかの子と同じように、倒れたと見える
newtype TooManyRestarts = TooManyRestarts Text
  deriving Show

instance Exception TooManyRestarts

-- 👀 Start every child, then wait for one to fall, ask `decide`, and
--    start again whoever it says. A supervisor is just an `IO ()`,
--    so it can be the child of another supervisor: a tree.
-- 子を全員始め、誰かが倒れるのを待ち、decide に聞いて、言われた子を
-- 始め直す。監督役もただの IO () なので、ほかの監督役の子になれる。木になる
supervise :: Plan -> IO ()
supervise plan = do
  running <- newIORef Map.empty
  let jobs  = Map.fromList [(childName c, work c) | c <- children plan]
      names = map childName (children plan)
      start name = for_ (Map.lookup name jobs) $ \job -> do
        thread <- async job
        modifyIORef running (Map.insert name thread)
      stop name = do
        threads <- readIORef running
        for_ (Map.lookup name threads) cancel
        modifyIORef running (Map.delete name)
      watch history = do
        threads <- readIORef running
        -- Wait for any child at once: `asum` tries each in turn (STM's
        -- `orElse`), and the first one that has ended gives its name.
        -- 全員を同時に待つ。asum が1人ずつ試し（STM の orElse）、
        -- 最初に終わっていた子が、名前を返す
        fallen <- atomically $
          asum [name <$ waitCatchSTM t | (name, t) <- Map.toList threads]
        now <- getMonotonicTime
        case decide (strategy plan) (intensity plan) now names fallen
                    history of
          (GiveUp, _) -> throwIO (TooManyRestarts fallen)
          (Restart again, history') -> do
            for_ again stop
            for_ again start
            watch history'
  (for_ names start >> watch [])
    `finally` (readIORef running >>= mapM_ cancel)
-- ANCHOR_END: supervise
-- ANCHOR_END: all
